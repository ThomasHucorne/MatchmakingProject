#include <Rcpp.h>
#include <queue>
#include <vector>
#include <string>
#include <unordered_map>

using namespace Rcpp;

/**
 * @title Best-First Gale–Shapley (Heap Version) — C++ Implementation
 *
 * @description
 * C++ version of the heap-based Gale–Shapley stable matching algorithm.
 * Uses a min-heap priority queue for proposal ordering.
 *
 * @param men_prefs A named list of men’s preference vectors.
 * @param women_prefs A named list of women’s preference vectors.
 *
 * @return A data.frame with matched couples.
 *
 * @export
 */
// [[Rcpp::export]]
DataFrame best_gs_bucket_cpp(List men_prefs,List women_prefs) {
  int n = men_prefs.size();

  // Extract names
  CharacterVector men_names   = men_prefs.names();
  CharacterVector women_names = women_prefs.names();

  // Build index maps: name -> index
  std::unordered_map<std::string,int> men_index;
  std::unordered_map<std::string,int> women_index;

  for (int i = 0; i < n; i++)
    men_index[ as<std::string>(men_names[i]) ] = i;
  for (int j = 0; j < n; j++)
    women_index[ as<std::string>(women_names[j]) ] = j;

  // Convert preferences to int indices
  std::vector<std::vector<int>> men_pref_num(n);
  std::vector<std::vector<int>> women_pref_num(n);

  for (int i = 0; i < n; i++) {
    CharacterVector prefs = men_prefs[i];
    int m = prefs.size();
    men_pref_num[i].resize(m);
    for (int k = 0; k < m; k++)
      men_pref_num[i][k] = women_index[ as<std::string>(prefs[k]) ];
  }
  for (int f = 0; f < n; f++) {
    CharacterVector prefs = women_prefs[f];
    int m = prefs.size();
    women_pref_num[f].resize(m);
    for (int k = 0; k < m; k++)
      women_pref_num[f][k] = men_index[ as<std::string>(prefs[k]) ];
  }

  // Build ranking table for women
  std::vector<std::vector<int>> women_rank(n, std::vector<int>(n, 0));

  for (int f = 0; f < n; f++) {
    for (int pos = 0; pos < n; pos++) {
      int man = women_pref_num[f][pos];
      women_rank[f][man] = pos;
    }
  }

  // State arrays
  std::vector<int> next_choice(n, 0);   // next woman index to propose
  std::vector<int> matching(n, -1);     // matching[h] = woman
  // woman f’s fiancé: find by scanning matching

  // Buckets: vector of vectors of pairs (man, woman)
  // bucket[priority] = proposals at that priority
  std::vector<std::vector<std::array<int,2>>> buckets(n);

  // Initialize bucket 0
  for (int h = 0; h < n; h++) {
    int f = men_pref_num[h][0];
    buckets[0].push_back({h, f});
  }

  // while some bucket is non-empty
  while (true) {

    // find first non-empty bucket
    int p = -1;
    for (int i = 0; i < n; i++) {
      if (!buckets[i].empty()) {
        p = i;
        break;
      }
    }

    if (p == -1) {
      break;
    }

    // pop one proposal
    auto prop = buckets[p].back();
    buckets[p].pop_back();

    int h = prop[0];
    int f = prop[1];

    // find current fiancé of f
    int current = -1;
    for (int i = 0; i < n; i++) {
      if (matching[i] == f) {
        current = i;
        break;
      }
    }

    if (current == -1) {
      // woman free
      matching[h] = f;

    } else {
      int rank_current = women_rank[f][current];
      int rank_new     = women_rank[f][h];

      if (rank_new < rank_current) {
        // woman prefers new
        matching[h] = f;
        matching[current] = -1;

        next_choice[current]++;
        if (next_choice[current] < n) {
          int f2 = men_pref_num[current][next_choice[current]];
          buckets[next_choice[current]].push_back({current, f2});
        }
      } else {
        // rejected
        next_choice[h]++;
        if (next_choice[h] < n) {
          int f2 = men_pref_num[h][next_choice[h]];
          buckets[next_choice[h]].push_back({h, f2});
        }
      }
    }
  }

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
