#include <Rcpp.h>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>
#include <limits>

using namespace Rcpp;

// Structure pour stocker les résultats du BFS
struct BFSResult {
  bool found;
  std::unordered_map<std::string, double> distance;
};

// Structure pour stocker les résultats du DFS
struct DFSResult {
  bool success;
  std::unordered_map<std::string, int> matching_donor;
  std::unordered_map<std::string, int> matching_receiver;
  std::unordered_map<std::string, double> distance;
};

// Fonction pour vérifier la compatibilité
// [[Rcpp::export]]
bool can_receive_cpp(const std::string& donor_type,
                     const std::string& recipient_type,
                     const NumericMatrix& compatibility_table,
                     const CharacterVector& blood_types) {
  int donor_idx = -1, recipient_idx = -1;

  for (int i = 0; i < blood_types.size(); i++) {
    if (std::string(blood_types[i]) == donor_type) donor_idx = i;
    if (std::string(blood_types[i]) == recipient_type) recipient_idx = i;
  }

  if (donor_idx == -1 || recipient_idx == -1) return false;

  return compatibility_table(donor_idx, recipient_idx) == 1;
}

// Construction du graphe de compatibilité
// [[Rcpp::export]]
List build_compatibility_graph_cpp(const IntegerVector& donors,
                                   const IntegerVector& receivers,
                                   const DataFrame& data,
                                   const NumericMatrix& compatibility_table,
                                   const CharacterVector& blood_types) {
  CharacterVector blood_type_col = data["blood_type"];
  List graph;

  for (int i = 0; i < donors.size(); i++) {
    int donor_id = donors[i];
    std::string donor_type = std::string(blood_type_col[donor_id - 1]);
    IntegerVector compatible_receivers;

    for (int j = 0; j < receivers.size(); j++) {
      int receiver_id = receivers[j];
      std::string receiver_type = std::string(blood_type_col[receiver_id - 1]);

      if (can_receive_cpp(donor_type, receiver_type, compatibility_table, blood_types)) {
        compatible_receivers.push_back(receiver_id);
      }
    }

    std::string key = std::to_string(donor_id);
    graph[key] = compatible_receivers;
  }

  return graph;
}

// BFS pour trouver des chemins augmentants
BFSResult bfs_hopcroft_karp_cpp(const List& graph,
                                const IntegerVector& donors,
                                const std::unordered_map<std::string, int>& matching_donor,
                                const std::unordered_map<std::string, int>& matching_receiver) {
  std::unordered_map<std::string, double> distance;
  std::queue<int> queue;

  // Ajouter tous les donneurs libres à la file
  for (int i = 0; i < donors.size(); i++) {
    int donor = donors[i];
    std::string donor_key = std::to_string(donor);

    auto it = matching_donor.find(donor_key);
    if (it == matching_donor.end() || it->second == NA_INTEGER) {
      distance[donor_key] = 0;
      queue.push(donor);
    } else {
      distance[donor_key] = std::numeric_limits<double>::infinity();
    }
  }

  distance["NIL"] = std::numeric_limits<double>::infinity();

  // BFS
  while (!queue.empty()) {
    int donor = queue.front();
    queue.pop();

    std::string donor_key = std::to_string(donor);

    if (distance[donor_key] < distance["NIL"]) {
      IntegerVector adjacent_receivers = graph[donor_key];

      for (int j = 0; j < adjacent_receivers.size(); j++) {
        int receiver = adjacent_receivers[j];
        std::string receiver_key = std::to_string(receiver);

        // Trouver le donneur associé à ce receveur
        std::string paired_donor_key = "NIL";
        auto it = matching_receiver.find(receiver_key);
        if (it != matching_receiver.end() && it->second != NA_INTEGER) {
          paired_donor_key = std::to_string(it->second);
        }

        // Si ce donneur n'a pas été visité
        if (distance.find(paired_donor_key) == distance.end() ||
            std::isinf(distance[paired_donor_key])) {
          distance[paired_donor_key] = distance[donor_key] + 1;

          if (paired_donor_key != "NIL") {
            queue.push(std::stoi(paired_donor_key));
          }
        }
      }
    }
  }

  BFSResult result;
  result.found = !std::isinf(distance["NIL"]);
  result.distance = distance;

  return result;
}

// DFS pour trouver et appliquer un chemin augmentant
DFSResult dfs_hopcroft_karp_cpp(int donor,
                                const List& graph,
                                std::unordered_map<std::string, double>& distance,
                                std::unordered_map<std::string, int>& matching_donor,
                                std::unordered_map<std::string, int>& matching_receiver) {
  DFSResult result;
  result.matching_donor = matching_donor;
  result.matching_receiver = matching_receiver;
  result.distance = distance;

  if (donor != NA_INTEGER) {
    std::string donor_key = std::to_string(donor);
    IntegerVector adjacent_receivers = graph[donor_key];

    for (int i = 0; i < adjacent_receivers.size(); i++) {
      int receiver = adjacent_receivers[i];
      std::string receiver_key = std::to_string(receiver);

      // Trouver le donneur associé à ce receveur
      std::string paired_donor_key = "NIL";
      int paired_donor = NA_INTEGER;
      auto it = matching_receiver.find(receiver_key);
      if (it != matching_receiver.end() && it->second != NA_INTEGER) {
        paired_donor = it->second;
        paired_donor_key = std::to_string(paired_donor);
      }

      // Vérifier la condition de distance
      if (distance[paired_donor_key] == distance[donor_key] + 1) {
        // Récursivement chercher un chemin augmentant
        DFSResult dfs_result = dfs_hopcroft_karp_cpp(paired_donor, graph, distance,
                                                     matching_donor, matching_receiver);

        if (dfs_result.success) {
          dfs_result.matching_receiver[receiver_key] = donor;
          dfs_result.matching_donor[donor_key] = receiver;
          return dfs_result;
        } else {
          matching_donor = dfs_result.matching_donor;
          matching_receiver = dfs_result.matching_receiver;
          distance = dfs_result.distance;
        }
      }
    }

    // Marquer ce donneur comme visité
    distance[donor_key] = std::numeric_limits<double>::infinity();
    result.success = false;
    result.matching_donor = matching_donor;
    result.matching_receiver = matching_receiver;
    result.distance = distance;
    return result;
  }

  // Cas de base : atteint NIL
  result.success = true;
  return result;
}

// Algorithme de Hopcroft-Karp
// [[Rcpp::export]]
List hopcroft_karp_cpp(const IntegerVector& donors,
                       const IntegerVector& receivers,
                       const DataFrame& data,
                       const NumericMatrix& compatibility_table,
                       const CharacterVector& blood_types) {

  // Rcout << "Building compatibility graph...\n";
  List graph = build_compatibility_graph_cpp(donors, receivers, data,
                                             compatibility_table, blood_types);

  // Initialisation des structures de matching
  std::unordered_map<std::string, int> matching_donor;
  std::unordered_map<std::string, int> matching_receiver;

  for (int i = 0; i < donors.size(); i++) {
    matching_donor[std::to_string(donors[i])] = NA_INTEGER;
  }

  for (int i = 0; i < receivers.size(); i++) {
    matching_receiver[std::to_string(receivers[i])] = NA_INTEGER;
  }

  int matching_size = 0;
  int iteration = 0;

  // Rcout << "Research of maximum matching...\n";

  // Tant qu'il existe un chemin augmentant
  while (iteration < 1000) {
    iteration++;
    BFSResult bfs_result = bfs_hopcroft_karp_cpp(graph, donors, matching_donor, matching_receiver);

    if (!bfs_result.found) {
      break;
    }

    // Pour chaque donneur libre, essayer de trouver un chemin augmentant
    for (int i = 0; i < donors.size(); i++) {
      int donor = donors[i];
      std::string donor_key = std::to_string(donor);

      if (matching_donor[donor_key] == NA_INTEGER) {
        DFSResult dfs_result = dfs_hopcroft_karp_cpp(donor, graph, bfs_result.distance,
                                                     matching_donor, matching_receiver);

        if (dfs_result.success) {
          matching_size++;
          matching_donor = dfs_result.matching_donor;
          matching_receiver = dfs_result.matching_receiver;
        }
      }
    }

    // Rcout << "  Iteration " << iteration << " : " << matching_size << " formed pairs\n";
  }

  // Rcout << "\nMatching ended in " << iteration << " iterations\n";

  // Convertir les maps en Lists R
  List matching_donor_list;
  List matching_receiver_list;

  for (const auto& pair : matching_donor) {
    matching_donor_list[pair.first] = pair.second;
  }

  for (const auto& pair : matching_receiver) {
    matching_receiver_list[pair.first] = pair.second;
  }

  return List::create(
    Named("matching_donor") = matching_donor_list,
    Named("matching_receiver") = matching_receiver_list,
    Named("matching_size") = matching_size,
    Named("graph") = graph
  );
}
