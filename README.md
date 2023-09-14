# Analyse de la relation entre les enquêtes de conjoncture et le PIB & protocole de prévision en temps réel

Créé par : Louise Phung (DG Trésor/SPMAE/Prev/Prev3)        
Créé le : 20/09/2022        

## Présentation du projet

Ce projet permet de reproduire les résultats du Document de Travail n°2023/2 **"Guide pratique des enquêtes de conjoncture et 
protocole de prévision en temps réel"** de la Direction générale du Trésor publié en septembre 2023.

**Ce projet s'intéresse à la relation entre le taux de croissance trimestriel du PIB français et les enquêtes de conjoncture,
ainsi qu'à la pertinence de ces dernières pour prédire la première estimation du taux de croissance trimestriel du PIB publiée 
par l'Insee environ 30 jours après la fin du trimestre concerné.**


Le fichier RMarkdown [`figures_doc_travail.Rmd`](./code/figures_doc_travail.Rmd) permet de reproduire tous les tableaux et graphiques
concernant l'analyse de la relation entre le PIB et les enquêtes de conjoncture dans la partie 4 du Document de Travail.        

Le script R [`script_pour_doc_travail.R`](./code/script_pour_doc_travail.R) permet d'approfondir cette analyse.

Enfin, le script R [`script_pour_doc_travail_prevision.R`](./code/script_pour_doc_travail_prevision.R) permet de reproduire tous
les résultats concernant les différentes expériences de simulations d'exercices de prévision qui sont détaillés dans la partie 5 du
Document de Travail.


## Enjeu du projet et de la publication

### Le Document de Travail

Ce document de travail présente une introduction globale aux enquêtes de conjoncture et à la prévision de court terme,
avec comme exemple la croissance du PIB trimestriel. 

**Pensé comme un manuel méthodologique, il a vocation à** : 
1) **améliorer l’interprétation et l’analyse des enquêtes de conjoncture**, qui sont très largement commentées mais souvent mal utilisées et, 
   
2) **proposer un protocole de prévision de court terme en temps réel** de la croissance du PIB grâce aux données d'enquêtes.

#### 1. Les enquêtes de conjoncture et l'analyse de leurs relations avec la croissance du PIB

Pour ce faire, ce document de travail offre partie 2 une présentation détaillée des principales enquêtes de conjoncture en France,
celles utilisées pour la construction de l’index des directeurs d’achats (PMI) de Standard & Poor’s, et des climats des affaires de la Banque de France et de l’Insee. 
Plusieurs tableaux récapitulatifs sont disponibles en annexe.       

Ce document de travail détaille également comment les indices devraient être interprétés parties 3 et 4 : en comparaison à leurs 
moyennes de long terme ou à un solde critique.

#### 2. La prévision à court terme (*nowcasting*) en temps réel de la croissance du PIB

Ce document présente ensuite partie 5 un cadre général d’utilisation de ces données pour la prédiction des agrégats économiques à court terme
(à horizon de un ou deux trimestres ; qu'on appelle parfois *nowcasting*), 
cadre visant à contourner les écueils les plus fréquents de ce genre d’exercice. 

En particulier, **le protocole de prévision *en temps réel*** est le seul qui permette de calculer correctement les métriques de performance, 
en simulant les exercices de prévision de la manière dont ils se seraient passés dans la réalité. 
Ce protocole est fondé sur :

1) **l’évaluation hors-échantillon** des modèles que l’on souhaite tester de manière itérative 
en agrandissant l’échantillon d’entraînement à chaque période et,
   
2) **l’utilisation de données millésimées** pour l’entraînement des modèles,
c’est-à-dire les données qui auraient été réellement disponibles à chaque exercice de prévision. 

Ce protocole permet également d’assurer la réplicabilité des résultats. 

Enfin, ce document de travail offre **plusieurs simulations d’exercices de prévision** pour illustrer la performance du 
protocole de prévision en temps réel, à l’aide de modèles linéaires avec des données d’enquêtes de conjoncture. 
Compte-tenu de la simplicité des modèles, leurs performances sont satisfaisantes.


### Le projet informatique

**Ce projet peut-être vu comme un tutoriel permettant de se familiariser avec** :
1) **L'analyse de données** et de leur pertinence pour éclairer les évolutions de l'activité économique ;
   
2) **La mise en place d'un protocole de prévision** simple, opérationnel et correct d'un point de vu statistique.

#### Protocole de prévision opérationnel

Dans le cadre de ce document de travail, **un travail de traitement de données a été réalisé pour reconstituer les millésimes du PIB** 
à partir du 4e trimestre 2007, période pour laquelle nous avons pu récupérer les publications successives des comptes trimestriels de la Nation. 
Ils sont utilisés dans la partie 5 du Document de Travail pour la simulation de plusieurs expériences de nowcasting suivant
le protocole de prévision en temps réel afin de mettre en évidence sa supériorité et la nécessité d’évaluer tout modèle 
à travers une telle procédure. 

Par ailleurs, afin de doter le Trésor d’un outil de prévision simple dont les résultats sont réplicables, **un travail d’ingénierie informatique
a également été réalisé pour intégrer ces simulations dans un programme opérationnel** qui permettra aux agents du Trésor 
de réaliser les futurs exercices de prévision de la croissance trimestrielle du PIB facilement et rapidement grâce à la génération automatique
de la série constituée des millésimes du PIB et la réalisation automatique du processus d’évaluation en temps réel.

On notera toutefois que **l'enjeu ici est bien de détailler le protocole de prévision en temps réel et de présenter la logique 
derrière un programme opérationnel à travers un exemple simple, c'est pourquoi les simulations s'arrêtent en 2019**. 

***Les conséquences de la crise du Covid-19 et les modèles réellement utilisés au sein de la Direction générale du Trésor 
seront traités et présentés dans une autre publication et dans un autre projet informatique.***


#### Objectif de reproductibilité des résultats

##### Les données
 
Les données brutes sous forme de `.RData` sont lues par les scripts R [`script_pour_doc_travail.R`](./code/script_pour_doc_travail.R) 
et [`script_pour_doc_travail_prevision.R`](./code/script_pour_doc_travail_prevision.R).
***Si vous souhaitez répliquer les résultats et réaliser des tests, les données brutes peuvent être mises à disposition
(voir la partie `Données utilisées`)***.        
  
Les transformations opérées sur les données brutes sont également explicitées dans ces deux scripts en toute transparence. 

Même si la mise à jour des données ne sera pas possible pour toute personne extérieure au Trésor 
  (et a fortiori tout agent extérieur à la sous-direction des Prévisions) puisque les fichiers sources (Excel ou csv) 
  dépendent des processus internes à la sous-direction des Prévisions (voir la partie `Données utilisées`), le code relatif à l'importation 
  des données depuis les fichiers sources est explicité dans les scripts R
  [`script_pour_doc_travail.R`](./code/script_pour_doc_travail.R) et [`script_pour_doc_travail_prevision.R`](./code/script_pour_doc_travail_prevision.R). 
  Ce code détaille la manière dont les données brutes sont importées, organisées et enregistrées sous forme de fichiers `.RData`.
***Ceux sont ces fichiers `.RData` qui peuvent être mis à disposition à la demande.***


##### Le code

Pour assurer la reproductibilité des résultats, le système de management de packages [`renv`](https://cran.r-project.org/web/packages/renv/vignettes/renv.html)
a été utilisé. Il permet d'installer tous les packages nécessaires au bon fonctionnement de ce projet aux bonnes versions afin
d'assurer que le comportement des fonctions appelées est conforme au comportement attendu au sein de ce projet. 

Ce projet est compatible avec les versions de R 4.2.2 et 4.2.3.
  

##### Reproduction des résultats avec Onyxia - SSP Cloud

Pour les agents publics, ***sous couvert d'avoir préalablement demandé les données***,
vous pouvez reproduire les résultats du Document de Travail, et réaliser des tests, avec l'utilisation d'une machine virtuelle
disponible sur [Onyxia - SSP Cloud](https://onyxia.lab.sspcloud.fr/home).

Vous devez créer un compte, si vous n'en avez pas déjà un, puis ouvrir un service RStudio. 
Dans le menu `Configuration RStudio`, allez à l'onglet `Service`  et choisissez comme `Version` de R `inseefrlab/onyxia-rstudio:r4.2.3`.
Si elle n'est pas disponible, précisez-la en cliquant sur `Custom image`. Lancez ensuite le service.

Suivez ensuite la procédure détaillée dans la partie `Première utilisation du projet` ci-dessous.



## Données utilisées

Trois types de données sont utilisées :
* **Les données dites "révisées" du PIB**, qui concerne la série du taux de croissance trimestriel du PIB en volume chaîné au prix de l’année précédente 
  publiée par l'Insee pour la première estimation du 1er trimestre 2023, le 28 avril 2023 ;    
* **Les données dites "non révisées" du PIB**, qui concerne la série constituée de toutes les premières estimations 
  du taux de croissance du PIB (en volume chaîné au prix de l’année précédente) à chaque trimestre à partir des *millésimes* récupérés à partir du 4e trimestre 2007 (voir le Document de Travail pp.***5 et 32***);
* **Les données d'enquêtes de conjoncture**, que l'on considère comme n'étant pas révisées (voir le Document de Travail p.***32***).
Parmi les données d'enquêtes de conjoncture, seuls les indices synthétiques des enquêtes Insee, Banque de France et
  Standard & Poor's (enquête PMI) sont utilisés. 
  Ces données ont été obtenues via le logiciel de données [DataInsight-Desktop 5.0](https://www.spglobal.com/en/research-insights/)  de Standard & Poor's 
  (précédemment IHS Markit) et extraites le 18 juillet 2023.


**L'analyse de la relation entre les enquêtes de conjoncture et la croissance du PIB** (fichier [`figures_doc_travail.Rmd`](./code/figures_doc_travail.Rmd) et 
script [`script_pour_doc_travail.R`](./code/script_pour_doc_travail.R)) utilise les données d'enquête ainsi que les données révisées
du PIB puisque l'on s'intéresse à la "vraie" relation (si tentée qu'elle puisse être observable) entre les enquêtes de conjoncture
et l'activité économique française, approximée par la mesure du PIB ; d'où l'usage des données révisées (voir le Document de Travail p.***19***).
L'analyse est réalisée sur la période du 2e trimestre 1998 au 4e trimestre 2019 inclus.


**Les expériences de simulations d'exercices de prévision de la croissance du PIB** (fichier [`script_pour_doc_travail_prevision.R`](./code/script_pour_doc_travail_prevision.R))
utilisent les données non-révisées du PIB ainsi que les données d'enquêtes de conjoncture. 
Puisque 1) les millésimes du PIB n'ont pu être récupérés qu'à partir du 4e trimestre 2007 et 2) les données antérieures du PIB
ne sont pas en volume chaîné au prix de l'année précédente, mais à prix constants, ces expériences sont réalisées sur la période
du 4e trimestre 2007 au 4e trimestre 2019. Les modèles sont entraînés sur la période du 4e trimestre 2007 au 3e trimestre 2015 inclus,
puis ils sont évalués sur la période du 4e trimestre 2015 au 4e trimestre 2019 inclus.

**La mise à jour des données pour ce projet est semi-automatique puisqu'elle dépend des processus internes à la sous-direction** :
- **Les données d'enquête** proviennent d'un fichier Excel (`donnees_pour_nowcasting_en_temps_reel.xlsx`) qui est mis à jour par une macro VBA (service du logiciel Data Insight 5.0).
  Ce fichier est directement lu par le code de ce programme ;
- **les données du PIB**, et de la comptabilité nationale trimestrielle en général, nous sont envoyées par l'Insee via 
      des fichiers Excel ou cvs, que nous sauvegardons dans différents dossiers. Le programme présenté dans ce projet va 
  ensuite automatiquement lire ces fichiers (sans avoir par exemple à renseigner quelles sont les dernières données disponibles,
  puisque l'intégralité des dossiers est scannée).
  
***A TRAITER: mise à disposition des données brutes au format .RData.***

## Première utilisation du projet

1) Télécharger le projet en local sur votre machine ;

2) Lancer le script [`project_initializator.R`](./project_initializator.R). Il permet de :
    - générer l'architecture du projet nécessaire à son bon fonctionnement (via la création de 2 dossiers) ;
    - de télécharger les packages (aux versions appropriées) nécessaires au bon fonctionnement du projet.
    
3) ***Si vous ne souhaitez pas seulement relire le code, mais également répliquer les résultats et réaliser des tests,
   les données peuvent être mises à disposition (voir la partie `Données utilisées`)*** ; il faudra ranger les fichiers .RData reçus dans le dossier [`data_doc_travail`](./data_doc_travail) ;
   
4) Selon ce que vous souhaitez faire, lancer :
    - le fichier RMarkdown [`figures_doc_travail.Rmd`](./code/figures_doc_travail.Rmd) ;
    - le script R [`script_pour_doc_travail.R`](./code/script_pour_doc_travail.R) ;
    - ou le script R [`script_pour_doc_travail_prevision.R`](./code/script_pour_doc_travail_prevision.R).
    
5) Veillez à bien lire les commentaires en début de script ; notamment en renseignant certaines constantes à `FALSE` si vous êtes
extérieurs à la Direction générale du Trésor.
    
A noter, les résultats (sortie HTLM résultant du lancement du fichier RMarkodwn ou sorties Excel générés par les deux scripts R)
sont disponibles dans le dossier [`output`](./output). 