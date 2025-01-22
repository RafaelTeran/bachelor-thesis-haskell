# Data Analysis and Machine Learning in Haskell

This repository contains the code and resources for my Bachelor’s Degree Thesis: **"Data Analysis and Machine Learning in Haskell"**. It demonstrates the creation of a database using the Wikipedia API and two key applications: a Naive Bayes Classifier and a PCA-based Recommender System.

**Nota para los usuarios hispanohablantes (Note for Spanish-speaking users):**  
Este repositorio contiene el código de mi Trabajo de Fin de Grado: **"Análisis de Datos y Aprendizaje Automático en Haskell"**. Las explicaciones completas sobre la teoría matemática y el desarrollo de los métodos presentados se encuentran en la versión completa de mi TFG, escrita en español y subida en este repositorio. La versión del código en español no tiene anotaciones porque el código se encuentra bien explicado en el PDF.

---

## Introduction

As part of my Bachelor’s Degree Thesis in Mathematics, titled **"Data Analysis and Machine Learning in Haskell"**, completed at the University of Seville, I explored the application of advanced mathematical concepts in data analysis and machine learning using Haskell, a functional programming language very similar to the way mathematicians think and well-suited for mathematical computations.

The aim of this work is to present two distinct methods of Machine Learning: **Naive Bayes**, a supervised learning system, and **Principal Component Analysis (PCA)**, a unsupervised learning system. To demonstrate their capabilities, practical applications have been developed for each method, supported by a custom database created using the Wikipedia API. The two applications are:
- A **Naive Bayes Classifier** to identify the language of a given text.  
- A **PCA-based Recommender System** to suggest related Wikipedia articles.

The complete version of my thesis, written in Spanish and included in this repository, provides an in-depth introduction to the mathematical theory underlying these methods, along with a detailed explanation of the development process for the scripts.

This thesis represents a strong foundation for further studies in Data Analysis, where I aim to deepen my knowledge and establish myself as a renowned professional in the field.

---

## Repository Structure

The repository includes two versions of the code:

### `src/`

This folder contains the source code divided into two main subfolders:

1. **`spanish/`**: The original code in Spanish, without annotations. This version matches the terminology and content described in the accompanying PDF, which is my complete Bachelor’s Degree Thesis. In the PDF, I also provide an extensive mathematical introduction and an in-depth explanation of the two methods used.

   - `base_de_datos/`
     - `GeneradorEnlaces.hs`: Generates random Wikipedia links.
     - `GeneradorTextos.hs`: Downloads texts from Wikipedia.
     - `GeneradorBaseDatos.hs`: Uses the other scripts to create the database.
   - `clasificador/`
     - `Clasificador.hs`: General Naive Bayes classifier implementation.
     - `ClasificadorIdioma.hs`: Example of the classifier applied to language detection using Wikipedia articles.
   - `recomendador/`
     - `Recomendador.hs`: General PCA-based recommender system implementation.
     - `RecomendadorArticulos.hs`: A recommender system for Wikipedia articles, suggesting related articles based on a specific one.

2. **`english/`**: Translated and annotated version of the code. This version is designed for an international audience and includes comments explaining the functionality of the scripts.

   - `database/`
     - `LinkGenerator.hs`: Generates random Wikipedia links.
     - `TextGenerator.hs`: Downloads texts from Wikipedia.
     - `DatabaseGenerator.hs`: Uses the other scripts to create the database.
   - `classifier/`
     - `Classifier.hs`: General Naive Bayes classifier implementation.
     - `LanguageClassifier.hs`: Example of the classifier applied to language detection using Wikipedia articles.
   - `recommender/`
     - `Recommender.hs`: General PCA-based recommender system implementation.
     - `ArticleRecommender.hs`: A recommender system for Wikipedia articles, suggesting related articles based on a specific one.

### `TfG_Análisis_de_datos_y_aprendizaje_automático_en_Haskell.pdf`

The complete thesis document in Spanish, detailing the methodology, implementation, and results. This includes:

- A comprehensive mathematical introduction covering both algebraical and statistical concepts used. These can be found in sections **"2. Clasificador Naive Bayes"** and **"3. Análisis de Componentes Principales"**, specifically subsections **2.1, 2.2, 3.1, 3.2, and 3.3**.
- Detailed theoretical explanations of the Naive Bayes Classifier and PCA Recommender System. These correspond again to sections **2. and 3.**, now subsections **2.3, 3.4, and 3.5**.
- An exhaustive explanation of the proposed implementations, corresponding to section **"4. Implementaciones"**
- An example for usage with a database of Wikipedia articles randomly selected, in section **"5. Aplicaciones"**. More precisely, the creation of the database in **5.1**, and the development of example applications using the created database in **5.2** for the classifier and **5.3** for the recommender.

---

## Prerequisites

To run the code, you need:

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/).
- Required Haskell libraries listed in "How to Use the Code"

After trying unsuccessfully installing some of the dependencies on a Windows machine, I strongly recommend running the code on a Linux system.

---

## How to Use the Code

1. **Clone the repository**  
   Clone the repository to your local machine and navigate to the project directory.

   ```bash
   git clone https://github.com/your-username/your-repository.git
   cd your-repository
   ```

2. **Choose the language version**  
   Navigate to the folder containing the desired version of the scripts:
   - For Spanish: `spanish/`
   - For English: `english/`

   Example:

   ```bash
   cd spanish
   ```

3. **Install dependencies**  
   The scripts require several Haskell libraries to run. Some libraries, such as `Data.List`, `Data.Ord`, `Data.Maybe`, and `Data.Either`, are standard and included with GHC, so you do not need to install them separately. However, other libraries must be installed using a package manager like `cabal` or `stack`.

   **Additional Dependencies:**
   - `Data.HashMap.Strict`
   - `Data.Hashable`
   - `Data.Char`
   - `Database.HDBC`
   - `Database.HDBC.Sqlite3`
   - `Network.HTTP.Client`
   - `Network.HTTP.Client.TLS`
   - `Data.Aeson`
   - `GHC.Generics`
   - `Numeric.LinearAlgebra.Data`
   - `Numeric.LinearAlgebra.HMatrix`
   - `Data.HashMap.Strict`
   - `Data.Text`

   To install dependencies with `cabal`, run:

   ```bash
   cabal update
   cabal install <library-name>
   ```

   Replace `<library-name>` with each dependency listed above. Make sure all dependencies are installed before proceeding.

4. **Running the Haskell Scripts**

To run the scripts, use a compatible GHC interpreter, such as `ghci`. The scripts include the functions necessary for interactive use. The functionality is further demonstrated in the "Examples and Results" section of this repository. Below, we provide a brief guide to executing the primary components:

**Naive Bayes Language Classifier**
1. Load the script:
   ```bash
   ghci> :l LanguageClassifier
   ```
   This will load the necessary functions for the language classification.

2. Create the database:
   ```bash
   ghci> languageIntros <number_of_article_introductions_per_language>
   ```
   - The database initializes with 30 languages using their ISO 639-1 codes. Users can modify this default setting if desired.

3. Use the classifier:
   - To classify the language of a text:
     ```bash
     ghci> languageClassifier "<text_to_identify>"
     ```
     This function returns the ISO code of the language most similar to the given text.
   
   - To obtain similarity results for all 30 languages:
     ```bash
     ghci> languageClassifierAll "<text_to_identify>"
     ```
     This function provides a list of all languages with their similarity scores.

**Principal Component Analysis (PCA) Article Recommender**
1. Load the script:
   ```bash
   ghci> :l ArticleRecommender
   ```
   This will load the necessary functions for article recommendation.

2. Create the database:
   ```bash
   ghci> articlesIntros <number_of_articles> <language_code>
   ```
   - For English, the script uses a predefined list of stopwords. Similarly, a Spanish version of this list is included in the Spanish-written code. Users can customize the language and stopword lists as needed.

3. Use the recommender:
   - To recommend articles based on a given Wikipedia article:
     ```bash
     ghci> articlesRecommender "<wikipedia_article_name>" <number_of_recommendations>
     ```
     This function downloads the specified article and provides a list of similar articles based on PCA analysis.

**Notes:**
- Both functionalities utilize a SQLite database for data management.
- For additional context and examples, refer to the "Examples and Results" section.

By following this guide, you can interactively explore and utilize the Naive Bayes Language Classifier and the PCA-based Article Recommender. Adjustments to default settings, such as the number of languages or articles, can be made to suit specific needs.

5. **Explore examples and results**  
   Examples of how to use both the recommender and classifier functions are provided in the "Examples and Results" section. These examples illustrate their application in practical scenarios.

6. **Explore and modify**  
   Feel free to explore and adapt the code for your own use cases. Contributions and feedback are welcome!

---

## Notes on Function and Parameter Names

- In the **Spanish version**, function and parameter names are consistent with those used in the PDF document to ensure clarity and alignment with the original work.
- In the **English version**, function and parameter names have been translated to enhance accessibility for an international audience. Annotations have been added to explain the code functionality.

---

## Examples and Results

These are some examples extracted form the PDF of the complete thesis (in spanish) and using ghci.

#### Naive Bayes Classifier:

```ghci
ghci> :l ClasificadorIdioma
[1 of 5] Compiling Clasificador     ( Clasificador.hs, interpreted )
[2 of 5] Compiling GeneradorEnlaces ( GeneradorEnlaces.hs, interpreted )
[3 of 5] Compiling GeneradorTextos  ( GeneradorTextos.hs, interpreted )
[4 of 5] Compiling GeneradorBaseDatos ( GeneradorBaseDatos.hs, interpreted )
[5 of 5] Compiling ClasificadorIdioma ( ClasificadorIdioma.hs, interpreted )
Ok, five modules loaded.
(0.79 secs,)

ghci> introsIdiomas 50
Base de datos creada con éxito.
Se han insertado 1500 elementos en la base de datos con éxito.
(1109.29 secs, 16,529,818,528 bytes)

ghci> clasificadorIdioma "Me alegro mucho de verte"
"es"
(1.85 secs, 1,328,026,208 bytes)

ghci> clasificadorIdiomaTodos "Me alegro mucho de verte"
[("es",1.4385796966713727e-23),("fr",2.0575862825810786e-24),
("pt",1.5006988488188862e-24),("en",1.3350031668054959e-24),
("nl",4.312475639514897e-25),("ca",1.868594793261105e-25),
("sv",7.40659682333777e-26),("ro",7.234569525937924e-26),
("it",2.457068448809503e-26),("tr",2.0575288642321292e-26),
("hu",1.6668801211580946e-26),("no",1.1000231787011853e-26),
("de",1.0814625409986618e-26),("ar",9.750177632771201e-27),
("ja",9.45650958601682e-27),("id",6.475369149049737e-27),
("fa",6.467582829508386e-27),("uk",5.923137433434641e-27),
("ru",5.703296873969729e-27),("pl",5.6597861219381146e-27),
("zh",5.320793439211851e-27),("vi",4.238956125487497e-27),
("he",4.1435008947009256e-27),("el",2.0638316732414866e-27),
("th",1.8556563972317497e-27),("ko",1.3629206500101532e-27),
("cs",1.2430390810619446e-27),("fi",9.051011209785674e-28),
("bn",7.818284158264162e-28),("hi",4.716927727546853e-28)]
(1.97 secs, 1,329,142,192 bytes)

ghci> clasificadorIdioma "I'm very happy to see you"
"en"
(2.09 secs, 1,522,266,088 bytes)

ghci> clasificadorIdioma "Estou muito feliz em ver você"
"pt"
(2.06 secs, 1,522,285,544 bytes)

ghci> clasificadorIdioma "Ich freue mich sehr, Sie zu sehen"
"de"
(2.28 secs, 1,716,540,048 bytes)

ghci> clasificadorIdioma "Bardzo się cieszę, że cię widzę"
"pl"
(2.92 secs, 1,522,319,184 bytes)

ghci> clasificadorIdioma "Sono molto felice di vederti"
"it"
(1.95 secs, 1,328,045,632 bytes)

ghci> clasificadorIdioma "Jeg er veldig glad for å se deg"
"no"
(2.61 secs, 1,910,765,376 bytes)
```

#### PCA Recommender:

```ghci
ghci> :l RecomendadorArticulos
[1 of 5] Compiling GeneradorEnlaces ( GeneradorEnlaces.hs, interpreted )
[2 of 5] Compiling GeneradorTextos  ( GeneradorTextos.hs, interpreted )
[3 of 5] Compiling GeneradorBaseDatos ( GeneradorBaseDatos.hs, interpreted )
[4 of 5] Compiling Recomendador     ( Recomendador.hs, interpreted )
[5 of 5] Compiling RecomendadorArticulos ( RecomendadorArticulos.hs, interpreted )
Ok, five modules loaded.
(1.14 secs,)

ghci> introsArticulos 1500 "es"
Base de datos creada con éxito.
Se han insertado 1500 elementos en la base de datos con éxito.
(929.72 secs, 16,246,341,576 bytes)

ghci> recomendadorArticulos "Usain Bolt" 10
Insertado en la base de datos con éxito.
["AlphaGo Zero","Serie Nacional de B\233isbol 1965-1966","Adri\225n Garc\237a","Stefania Belmondo","Tatsuhiro Yonemitsu","Indonesia en los Juegos Ol\237mpicos de Roma 1960","Mauritania en los Juegos Paral\237mpicos de S\237dney 2000","Mal\237 en los Juegos Ol\237mpicos de Londres 2012","Miguel Martinez (ciclista)","Lilou Llu\237s Valette"]
(2.24 secs, 743,440,056 bytes)

ghci> recomendadorArticulos "Sevilla" 10
Insertado en la base de datos con éxito.
["Sitio de Ciudad Rodrigo (1707)","Sombrerete","Neckarsulm","Casino Militar de Melilla","La Encarnaci\243n (desambiguaci\243n)","Novoros\237isk","Dom\382ale","Hoery\335ng","\346winouj\347cie","Circunscripci\243n electoral de Zaragoza"]
(3.24 secs, 737,889,256 bytes)

ghci> recomendadorArticulos "Manzana" 10
Insertado en la base de datos con éxito.
["Phalonidia lydiae","Messor reticuliventris","Derolus griseonotatus","Choristoneura occidentalis","Eucalyptus regnans","Nyssodrysternum fulminans","Phalonidia docilis","Brancasaurus","Strangalia beltii","Xixuthrus terribilis"]
(2.24 secs, 743,213,920 bytes)

ghci> recomendadorArticulos "Albert Einstein" 10
Insertado en la base de datos con éxito.
["Descubrimientos de pies humanos en el Mar de los Salish","Maeve Brennan","Andrew N. Dugger","Shave and a Haircut","John Smith (luchador ol\237mpico)","Pr\243tesis de retina Argus","Atlantismo","Transporte Presidente Pinto","Isla de hielo de Fletcher","Samantha Arsenault"]
(2.52 secs, 746,537,408 bytes)

ghci> recomendadorArticulos "España" 10
Insertado en la base de datos con éxito.
["\205ndice de referencia de pr\233stamos hipotecarios","C\233sar E. Arroyo","Invasi\243n de Portugal (1807)","Jos\233 Gautier Ben\237tez","Comisiones Obreras de Euskadi","Circunscripci\243n electoral de Zaragoza","Condado del Puente","Menorca (gallina)","Archivo Hist\243rico Minero de la Fundaci\243n R\237o Tinto","Boeng Krum"]
(2.30 secs, 753,417,248 bytes)
```
---

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

If you have any questions or suggestions, feel free to open an issue or contact me through the repository.



