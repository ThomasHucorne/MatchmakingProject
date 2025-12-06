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
DataFrame best_gs_heap_cpp(List men_prefs, List women_prefs) {

  struct Heap {
    int man;
    int woman;
    int prio;

    bool operator>(const Heap &other) const {
      return prio > other.prio;
    }
  };

  int n = men_prefs.size();

  CharacterVector men_names = men_prefs.names();
  CharacterVector women_names = women_prefs.names();

  std::unordered_map<std::string,int> men_index;
  std::unordered_map<std::string,int> women_index;

  // Build index tables
  for (int i = 0; i < n; ++i) {
    men_index[ as<std::string>(men_names[i]) ] = i;
  }
  for (int j = 0; j < n; ++j) {
    women_index[ as<std::string>(women_names[j]) ] = j;
  }

  // Convert preferences to numeric
  std::vector<std::vector<int>> men_pref_num(n);
  for (int i = 0; i < n; ++i) {
    CharacterVector v = men_prefs[i];
    for (int t = 0; t < v.size(); ++t) {
      men_pref_num[i].push_back( women_index[ as<std::string>(v[t]) ] );
    }
  }

  // Women ranking maps
  std::vector<std::unordered_map<int,int>> women_rank(n);
  for (int i = 0; i < n; ++i) {
    CharacterVector v = women_prefs[i];
    for (int t = 0; t < v.size(); ++t) {
      int man_id = men_index[ as<std::string>(v[t]) ];
      women_rank[i][man_id] = t + 1;
    }
  }
  // Track: next woman each man will propose to
  // Matching: matching[h] = woman index
  std::vector<int> next_choice(n, 1);
  std::vector<int> matching(n, -1);

  // Min-heap
  std::priority_queue<Heap, std::vector<Heap>, std::greater<Heap>> heap;

  // Initial proposals (first choice for each man)
  for (int h = 0; h < n; ++h) {
    if (!men_pref_num[h].empty()) {
      heap.push(Heap{h, men_pref_num[h][0], 1});
    }
  }

  // Main loop
  while (!heap.empty()) {
    // Deleting the proposition of matching from heap
    Heap top = heap.top();
    heap.pop();

    int h = top.man;
    int f = top.woman;

    // Check if woman f already matched
    int current = -1;
    for (int x = 0; x < n; ++x) {
      if (matching[x] == f) {
        current = x;
        break;
      }
    }

    // Woman is free -> accept
    if (current == -1) {

      matching[h] = f;

    } else {

      int rank_current = women_rank[f][current];
      int rank_new     = women_rank[f][h];

      // f prefers h over current fiancé
      if (rank_new < rank_current) {

        matching[h] = f;
        matching[current] = -1;

        next_choice[current]++;
        if (next_choice[current] <= (int)men_pref_num[current].size()) {
          int f2 = men_pref_num[current][next_choice[current] - 1];
          heap.push(Heap{current, f2, next_choice[current]});
        }

      } else {
        // Rejected -> propose to next woman
        next_choice[h]++;
        if (next_choice[h] <= (int)men_pref_num[h].size()) {
          int f2 = men_pref_num[h][next_choice[h] - 1];
          heap.push(Heap{h, f2, next_choice[h]});
        }
      }
    }
  }

  // Convert matching back to names
  CharacterVector out_men(n), out_women(n);
  for (int h = 0; h < n; ++h) {
    out_men[h] = men_names[h];
    int f = matching[h];
    out_women[h] = (f >= 0) ? women_names[f] : NA_STRING;
  }

  return DataFrame::create(
    _["Man"]   = out_men,
    _["Woman"] = out_women,
    _["stringsAsFactors"] = false
  );
}
