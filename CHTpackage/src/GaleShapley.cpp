#include <Rcpp.h>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
using namespace Rcpp;

//' Gale-Shapley Stable Matching Algorithm
 //'
 //' Implements the Gale-Shapley algorithm for stable matching using C++.
 //'
 //' @param men_prefs A named list of men's preferences (each element is a character vector of women names)
 //' @param women_prefs A named list of women's preferences (each element is a character vector of men names)
 //' @return A data.frame with columns "Man" and "Woman" representing the stable matches
 //' @examples
 //' men_prefs <- list(
 //'   A = c("Z", "X", "Y"),
 //'   B = c("Y", "X", "Z"),
 //'   C = c("X", "Z", "Y")
 //' )
 //' women_prefs <- list(
 //'   X = c("B", "A", "C"),
 //'   Y = c("A", "B", "C"),
 //'   Z = c("A", "C", "B")
 //' )
 //' gale_shapley_cpp(men_prefs, women_prefs)
 //' @export
 // [[Rcpp::export]]
 DataFrame gale_shapley_cpp(List men_prefs, List women_prefs) {
   // Récupérer les noms des hommes et des femmes
   CharacterVector men_names = men_prefs.names();
   CharacterVector women_names = women_prefs.names();
   int n_men = men_names.size();

   // Créer un ensemble des hommes libres
   std::set<std::string> free_men;
   for (int i = 0; i < n_men; i++) {
     free_men.insert(as<std::string>(men_names[i]));
   }

   // Map pour les fiançailles (femme -> homme)
   std::map<std::string, std::string> engaged;

   // Map pour le prochain index de proposition de chaque homme
   std::map<std::string, int> next_proposal;
   for (int i = 0; i < n_men; i++) {
     std::string man = as<std::string>(men_names[i]);
     next_proposal[man] = 0;
   }

   // Créer la structure de rang pour chaque femme (femme -> (homme -> rang))
   std::map<std::string, std::map<std::string, int>> rank;
   for (int i = 0; i < women_names.size(); i++) {
     std::string woman = as<std::string>(women_names[i]);
     CharacterVector prefs = as<CharacterVector>(women_prefs[woman]);
     for (int j = 0; j < prefs.size(); j++) {
       std::string man = as<std::string>(prefs[j]);
       rank[woman][man] = j;
     }
   }

   // Algorithme principal
   while (!free_men.empty()) {
     // Prendre le premier homme libre
     std::string man = *free_men.begin();

     // Obtenir les préférences de cet homme
     CharacterVector man_pref_vec = as<CharacterVector>(men_prefs[man]);

     // Vérifier si l'homme a encore des femmes à proposer
     if (next_proposal[man] >= man_pref_vec.size()) {
       // Plus de femmes à proposer, retirer de la liste
       free_men.erase(man);
       continue;
     }

     // La femme à qui il propose
     std::string woman = as<std::string>(man_pref_vec[next_proposal[man]]);

     // L'homme fait sa proposition
     if (engaged.find(woman) == engaged.end()) {
       // La femme est libre, elle accepte
       engaged[woman] = man;
       free_men.erase(man);
     } else {
       // La femme est déjà fiancée
       std::string current = engaged[woman];

       // Comparer les rangs
       if (rank[woman][man] < rank[woman][current]) {
         // La femme préfère le nouveau prétendant
         engaged[woman] = man;
         free_men.insert(current);
         free_men.erase(man);
       }
       // Sinon, la femme rejette le prétendant (il reste libre)
     }

     // Incrémenter l'index de proposition
     next_proposal[man]++;
   }

   // Construire le DataFrame de résultat
   std::vector<std::string> result_men;
   std::vector<std::string> result_women;

   for (auto it = engaged.begin(); it != engaged.end(); ++it) {
     result_men.push_back(it->second);    // Man
     result_women.push_back(it->first);   // Woman
   }

   return DataFrame::create(
     Named("Man") = result_men,
     Named("Woman") = result_women,
     _["stringsAsFactors"] = false
   );
 }
