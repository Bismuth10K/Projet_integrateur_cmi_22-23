# Projet intégrateur CMI 2022-2023
Projet intégrateur des CMI de l'Université Bretagne Sud (année scolaire 2022/2023).

Notre projet consiste en l'analyse et la modélisation du réchauffement climatique.
Nous nous concentrerons principalement sur :
- la masse de l'antarctique
- le niveau de CO2
- l'augmentation de la température

Datasets fournis par la [NASA](https://climate.nasa.gov/).
Les variables sont expliquées dans le fichier [description_data.txt](./description_data.txt)

## Structure
Le dossier `datasets_nasa` contient les datasets récupérés sur le site de la NASA précédemment cité. On y trouve un dossier `cleaned_datasets` contenant les datasets précédents, mais nettoyés de façon à correspondre au projet.

Le dossier `descript_tables` contient une description détaillé pour chaque table.

Dans le dossier `script`, on y trouvera les scripts ayant contribué à nos recherches et aux modélisations.
Nous pourrons aussi y trouver un dossier `interface` contenant le script de l'interface *shiny*. 

## Installation
Pour cloner le repo, utilisez la commande suivante dans le dossier de votre choix :

```
$ git clone https://github.com/Bismuth10K/Projet_integrateur_cmi_22-23.git
```

Nous vous recommandons d'ouvrir le [projet](./Projet_integrateur_cmi_22-23.Rproj) avec Rstudio afin d'avoir plus de facilité de lecture et d'exécution .

Plusieurs packages seront nécessaires à l'exécution des scripts, Rstudio vous préviendra lorsqu'un package sera éventuellement manquant.

### Interface SHINY
Ouvrez le script [app.R](scripts/interface/app.R) et exécutez le avec les packages installés, une page web s'ouvrira et vous pourrez y visualiser les données.