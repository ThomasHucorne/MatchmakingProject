#include <Rcpp.h>
#include <queue>
#include <vector>
#include <string>
#include <unordered_map>
using namespace Rcpp;

//' Best-First Gale-Shapley (Bucket Version)
 //'
 //' C++ implementation of the bucket-based Gale-Shapley stable matching algorithm.
 //' Uses a bucket priority queue for proposal ordering.
 //'
 //' @param men_prefs A named list of men's preference vectors
 //' @param women_prefs A named list of women's preference vectors
 //' @return A data.frame with matched couples (columns: Man, Woman)
 //' @export
 // [[Rcpp::export]]
 DataFrame best_gs_bucket_cpp(List men_prefs, List women_prefs) {
   int n = men_prefs.size();
   // Retrieve R names
   CharacterVector men_names   = men_prefs.names();
   CharacterVector women_names = women_prefs.names();

   // Convert men preferences to numeric indices
   std::vector<std::vector<int>> men_pref_num(n);
   std::map<String,int> women_index;
   for (int i = 0; i < n; i++)
     women_index[women_names[i]] = i;
   for (int h = 0; h < n; h++) {
     CharacterVector v = men_prefs[h];
     int m = v.size();
     men_pref_num[h].resize(m);
     for (int j = 0; j < m; j++)
       men_pref_num[h][j] = women_index[v[j]];
   }

   // Convert women preferences
   std::vector<std::vector<int>> women_pref_num(n);
   std::map<String,int> men_index;
   for (int i = 0; i < n; i++)
     men_index[men_names[i]] = i;
   for (int f = 0; f < n; f++) {
     CharacterVector v = women_prefs[f];
     int m = v.size();
     women_pref_num[f].resize(m);
     for (int j = 0; j < m; j++)
       women_pref_num[f][j] = men_index[v[j]];
   }

   // Build women_rank
   std::vector<std::vector<int>> women_rank(n, std::vector<int>(n));
   for (int f = 0; f < n; f++) {
     for (int pos = 0; pos < n; pos++) {
       int man = women_pref_num[f][pos];
       women_rank[f][man] = pos;     // lower = better
     }
   }

   // State arrays
   std::vector<int> next_choice(n, 0);
   std::vector<int> matching(n, -1);

   // Buckets : vector of vector of pairs (h,f)
   std::vector<std::vector<std::pair<int,int>>> buckets(n);

   // Initialize bucket #0
   for (int h = 0; h < n; h++) {
     int f = men_pref_num[h][0];
     buckets[0].push_back({h, f});
   }

   // Main loop
   while (true) {
     // Find first non-empty bucket
     int p = -1;
     for (int i = 0; i < n; i++) {
       if (!buckets[i].empty()) {
         p = i;
         break;
       }
     }
     if (p == -1) break;

     // Pop proposal
     auto prop = buckets[p].back();
     buckets[p].pop_back();
     int h = prop.first;
     int f = prop.second;

     // Find current fiancé of f
     int current = -1;
     for (int i = 0; i < n; i++) {
       if (matching[i] == f) {
         current = i;
         break;
       }
     }

     if (current == -1) {
       // free woman -> accept
       matching[h] = f;
     }
     else {
       // woman compares men
       int rc = women_rank[f][current];
       int rn = women_rank[f][h];
       if (rn < rc) {
         // replace fiancé
         matching[h] = f;
         matching[current] = -1;
         next_choice[current]++;
         int nc = next_choice[current];
         if (nc < n) {
           int f2 = men_pref_num[current][nc];
           buckets[nc].push_back({current, f2});
         }
       }
       else {
         // reject h
         next_choice[h]++;
         int nh = next_choice[h];
         if (nh < n) {
           int f2 = men_pref_num[h][nh];
           buckets[nh].push_back({h, f2});
         }
       }
     }
   }

   // Build final output data.frame
   CharacterVector out_women(n);
   for (int h = 0; h < n; h++) {
     int f = matching[h];
     out_women[h] = women_names[f];
   }

   return DataFrame::create(
     _["Man"] = men_names,
     _["Woman"] = out_women,
     _["stringsAsFactors"] = false
   );
 }
