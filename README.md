# Réflexion projet – 09 décembre 2018


- Récupération de données contextuelles
- Récupérer les données météo
- Récupérer données autres, transports 
  - Jan l’a fait pour le métro
Calculer le trajet optimal pour aller de A à B : fourchette de durées….  
Construction d’une matrice 
A faire pour le Bus
Données populationnelles : population, description socio-économiques, profil électoral
Données d’équipements : sites touristiques, stades, bureaux, écoles, 
Evènements d’intérêts : matchs sportifs, manifestations politiques, etc.
Jours fériés et de fête

## Nettoyage base
Incohérences ou valeurs aberrantes (ex. Anomalie délai utilisation vélo)
Quelle unité géo utiliser ?
Dimension temporelle

## Description des données (exemples de questions) :
Description de la base (statistiques de base) : 
Analyse centrer les bornes : montée en charge dans son utilisation, bornes en panne
Description des trajectoires : 
départ-arrivée les plus fréquents
Quelles distances sont effectuées : segments x durée 
Recherche d’une saisonnalité dans les trajets ?
Profils des individus qui utilisent les vélos en libre-service : âge, sexe, subscriber ou pas, évolution dans le temps
Sur-usage ou sous-usage de certaines stations ?

## Questions qu’on pourrait explorer :
Prédire l’utilisation des bornes pour aller à certaines destinations ?
Prédire la destination en fonction des bornes de départ ?
Proposer de nouvelles localisations de bornes ?
Interconnexion avec d’autres formes de transport ?
Quelle part du vélo parmi les transports ?
Est-ce que l’offre du vélo a changé dans le temps ?
Est-ce que les usages du vélo par les particuliers a changé dans le temps ?
Quel usage en fonction de la météo, des circonstances
Typologie de stations et/ou individus
Quel probabilité d’utiliser le vélo plutôt qu’un autre mode de transport pour aller à une destination donnée
Variables de stratification : météo, type de destination…

## Stratégie d’analyse
Commencer par travailler sur un fichier trimestriel avant de passer à la table longitudinale

## Mode de travail en équipe
- Github partagé
- Travailler en Rnotebook 
- Base de travail commune
- Travailler en data.table majoritairement
- Sorties de fichier en .csv et en Rds


