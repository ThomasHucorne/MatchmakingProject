/**
 * @title Best-First Gale–Shapley Stable Matching (C++ Implementation)
 *
 * @description
 * Implements a heap-based, best-first version of the Gale–Shapley algorithm
 * using a C++ priority queue (min-heap).
 *
 * Each proposal is inserted into a heap with priority equal to the rank
 * (1 = top choice, 2 = second choice…) and the algorithm always extracts
 * the proposal with the lowest priority value first.
 *
 * This produces the same stable matching as classical Gale–Shapley but
 * processes proposals in a strictly priority-driven order.
 *
 * @param men_prefs A named list where each entry is a character vector of
 * women ordered from most preferred to least preferred.
 *
 * @param women_prefs A named list where each entry is a character vector of
 * men ordered from most preferred to least preferred.
 *
 * @return A data.frame with two character columns:
 * \itemize{
 *   \item \code{Man} — man's name
 *   \item \code{Woman} — matched woman
 * }
 *
 * @examples
 * men_prefs <- list(
 *   A = c("X","Y","Z"),
 *   B = c("Y","X","Z"),
 *   C = c("X","Z","Y")
 * )
 *
 * women_prefs <- list(
 *   X = c("B","A","C"),
 *   Y = c("A","B","C"),
 *   Z = c("A","C","B")
 * )
 *
 * best_gs_heap_cpp(men_prefs, women_prefs)
 *
 * @seealso
 * \code{\link{gale_shapley}} — classic Gale–Shapley in R
 * \code{\link{best_gs_heap}} — heap version entirely in R
 *
 * @export
 */
// [[Rcpp::export]]
Rcpp::DataFrame best_gs_heap_cpp(Rcpp::List men_prefs, Rcpp::List women_prefs) {

  using namespace Rcpp;
  using namespace std;

  struct Node {
    int man;
    int woman;
    int prio;
    bool operator>(const Node &other) const { return prio > other.prio; }
  };

  int n = men_prefs.size();

  // Extract names
  CharacterVector men_names = men_prefs.names();
  CharacterVector women_names = women_prefs.names();

  unordered_map<string,int> men_index, women_index;

  for (int i=0; i<n; i++)
    men_index[ as<string>(men_names[i]) ] = i;

  for (int j=0; j<women_prefs.size(); j++)
    women_index[ as<string>(women_names[j]) ] = j;

  // Convert men's preferences to numeric indices
  vector< vector<int> > men_pref_num(n);
  for (int i=0; i<n; i++) {
    CharacterVector v = men_prefs[i];
    int k = v.size();
    men_pref_num[i].reserve(k);
    for (int t=0; t<k; t++)
      men_pref_num[i].push_back( women_index[ as<string>(v[t]) ] );
  }

  // Build women's ranking tables
  vector< unordered_map<int,int> > women_rank(n);
  for (int i=0; i<n; i++) {
    CharacterVector v = women_prefs[i];
    int k = v.size();
    for (int t=0; t<k; t++) {
      int man_id = men_index[ as<string>(v[t]) ];
      women_rank[i][man_id] = t+1;
    }
  }

  vector<int> next_choice(n, 1);
  vector<int> matching(n, -1);

  // MIN-heap priority queue
  priority_queue<Node, vector<Node>, greater<Node>> heap;

  // Initialize heap with top choices
  for (int h=0; h<n; h++)
    heap.push( Node{h, men_pref_num[h][0], 1} );

  while (!heap.empty()) {

    Node top = heap.top();
    heap.pop();

    int h = top.man;
    int f = top.woman;

    // Find current fiancé of f
    int current = -1;
    for (int x=0; x<n; x++)
      if (matching[x] == f)
        current = x;

      if (current == -1) {
        matching[h] = f;
      } else {
        int rank_current = women_rank[f][current];
        int rank_new     = women_rank[f][h];

        if (rank_new < rank_current) {
          matching[h] = f;
          matching[current] = -1;

          next_choice[current]++;
          if (next_choice[current] <= (int)men_pref_num[current].size()) {
            int f2 = men_pref_num[current][ next_choice[current]-1 ];
            heap.push( Node{current, f2, next_choice[current]} );
          }
        } else {
          next_choice[h]++;
          if (next_choice[h] <= (int)men_pref_num[h].size()) {
            int f2 = men_pref_num[h][ next_choice[h]-1 ];
            heap.push( Node{h, f2, next_choice[h]} );
          }
        }
      }
  }

  // Convert output
  CharacterVector out_men(n);
  CharacterVector out_women(n);

  for (int h=0; h<n; h++) {
    out_men[h] = men_names[h];
    out_women[h] = women_names[ matching[h] ];
  }

  return DataFrame::create(
    _["Man"] = out_men,
    _["Woman"] = out_women,
    _["stringsAsFactors"] = false
  );
}
