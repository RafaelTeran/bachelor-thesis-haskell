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

The database mentioned is an external relational SQLite database, stored as a standalone file. Each record consists of an article's language, title, and introduction text, all extracted from randomly chosen Wikipedia articles.

The complete version of my thesis, written in Spanish and included in this repository, provides an in-depth introduction to the mathematical theory underlying these methods, along with a detailed explanation of the development process for the scripts.

This thesis represents a strong foundation for further studies in Data Analysis, where I aim to deepen my knowledge and establish myself as a renowned professional in the field.

---

## Repository Structure

The repository includes two versions of the code:

### `src/`

The source code is organized into two main subfolders:

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
ghci> :l LanguageClassifier
[1 of 5] Compiling Classifier       ( Classifier.hs, interpreted )
[2 of 5] Compiling LinkGenerator    ( LinkGenerator.hs, interpreted )
[3 of 5] Compiling TextGenerator    ( TextGenerator.hs, interpreted )
[4 of 5] Compiling DataBaseGenerator ( DataBaseGenerator.hs, interpreted )
[5 of 5] Compiling LanguageClassifier ( LanguageClassifier.hs, interpreted )
Ok, five modules loaded.
(0.18 secs,)

ghci> languageIntros 50
Database successfully created.
Successfully inserted 30 entries into the database.
(1109.29 secs, 16,529,818,528 bytes)

ghci> languageClassifierAll "the dog has eaten my homework"
[("en",9.251331851016666e-27),("es",2.2939869088132698e-30),("fr",1.403877758920367e-30),("pt",4.432505042343432e-31),("ru",2.1620706048558024e-31),("de",2.0768171147957485e-31),("cs",1.892527591502138e-31),("ja",1.8754899432188765e-31),("tr",1.339414344418927e-31),("fa",1.2750183775327473e-31),("fi",1.1511398310057148e-31),("zh",1.0737491501268948e-31),("pl",1.020035684817893e-31),("it",9.723138685917976e-32),("sv",9.627714869421418e-32),("nl",8.816588393162065e-32),("he",8.054919869948755e-32),("ro",7.88552219243598e-32),("hi",7.631979224454328e-32),("uk",7.610870781267958e-32),("ko",5.340492326990447e-32),("no",5.171623449928648e-32),("ar",4.87665748868786e-32),("vi",3.870875290731309e-32),("el",3.857553111066574e-32),("th",3.6767940421356004e-32),("hu",3.6200305694047475e-32),("id",3.297224952812741e-32),("ca",2.214063895108989e-32),("bn",1.6114735390125858e-32)]
(1.94 secs, 1,401,371,984 bytes)

ghci> languageClassifierAll "el perro se ha comido mis deberes"
[("es",8.628169490183964e-33),("ca",5.133938728833182e-34),("ro",4.299213679270386e-35),("pt",4.184717459757538e-35),("fr",1.94309348861031e-35),("en",1.513198281365557e-35),("cs",1.3341252219912483e-35),("it",3.5761734126662986e-36),("fi",3.018841066234622e-36),("de",2.5094455229528134e-36),("ja",2.3355457438405972e-36),("zh",1.3399253136917638e-36),("ru",1.305882078746468e-36),("sv",8.717913424446208e-37),("fa",7.723732887076093e-37),("hu",6.604047351844521e-37),("pl",6.13068531943295e-37),("ar",5.937512922562016e-37),("nl",5.361844648950366e-37),("he",4.8613226006667445e-37),("vi",4.671979639519763e-37),("uk",4.623802129541536e-37),("tr",4.0782586881110235e-37),("id",4.009076592593674e-37),("ko",3.273764682762488e-37),("no",3.1078720763495153e-37),("el",2.291824469793233e-37),("th",2.2596944553171247e-37),("bn",1.9391980012185147e-37),("hi",1.2989762746331858e-37)]
(2.09 secs, 1,579,712,072 bytes)

ghci> languageClassifier "le chien a mangé mes devoirs"
"fr"
(1.90 secs, 1,400,231,000 bytes)

ghci> languageClassifier "Der Hund hat meine Hausaufgaben gefressen"
"de"
(1.88 secs, 1,400,294,232 bytes)

ghci> languageClassifier "ο σκύλος έφαγε την εργασία μου"
"el"
(1.87 secs, 1,400,240,712 bytes)

ghci> languageClassifier "con chó đã ăn bài tập về nhà của tôi"
"vi"
(2.58 secs, 2,113,619,368 bytes)

ghci> languageClassifier "o cachorro comeu meu dever de casa"
"pt"
(2.13 secs, 1,578,597,592 bytes)
```

#### PCA Recommender:

```ghci
ghci> :l ArticleRecommender
[1 of 5] Compiling LinkGenerator    ( LinkGenerator.hs, interpreted )
[2 of 5] Compiling Recommender      ( Recommender.hs, interpreted )
[3 of 5] Compiling TextGenerator    ( TextGenerator.hs, interpreted )
[4 of 5] Compiling DataBaseGenerator ( DataBaseGenerator.hs, interpreted )
[5 of 5] Compiling ArticleRecommender ( ArticleRecommender.hs, interpreted )
Ok, five modules loaded.
(2.24 secs,)

ghci> articleIntros 1500 "en"
Database successfully created.
Successfully inserted 1500 entries into the database.
(1135.68 secs, 16,272,789,992 bytes)

ghci> articleRecommender "Usain Bolt" 10
Successfully inserted into the database.
["Maki Tabata","Val\233rie Grenier","2023 IIHF World Championship final","Supercard of Honor (2023)","Jason Warner","2007 World Women's Curling Championship","Georgina Garc\237a P\233rez","WADP Numbering System","Geiny P\225jaro","List of baseball players who are Olympic gold medalists and World Series champions"]
(2.09 secs, 788,098,416 bytes)

ghci> articleRecommender "London" 10
Successfully inserted into the database.
["Hat Yai","Sun Valley, Idaho","Susaki, K\333chi","Nizhny Novgorod City Rail","MKM Stadium","List of people named Shemaiah in the Bible","Street of the Prophets","Pedro Vicente Maldonado (city)","Gavi\227o Kyikatej\234 Futebol Clube","Kalmiuske"]
(1.93 secs, 791,643,008 bytes)

ghci> articleRecommender "New York" 10
Successfully inserted into the database.
["Metuchen, New Jersey","2001 New York City Marathon","Surprise Lake Camp","New Jersey Route 81","Stanley, Inc.","Minnesota State Highway 237","The Emperor's Tomb","Crookwell railway station","Chirlane McCray","Union Bridge Company"]
(1.85 secs, 790,391,128 bytes)

ghci> articleRecommender "Banana" 10
Successfully inserted into the database.
["Encyclia fehlingii","Trithuria","Monoclea forsteri","Arahura","2014 FIBA 3x3 World Tour Manila Masters","Chi Mei Corporation","Sophie Becker","Chrysodeixis eriosoma","Diplopseustis selenalis","Top Spin 3"]
(1.84 secs, 793,214,896 bytes)

ghci> articleRecommender "Albert Einstein" 10
Successfully inserted into the database.
["WADP Numbering System","Tony De Vit","2019 in UFC","Motor Trend Car of the Year","Little Finlandia Prize","Black Forest Clock Association","Kraken (roller coaster)","Anna Leopoldovna","AP Macroeconomics","Stefan Holtz"]
(1.91 secs, 796,784,624 bytes)

ghci> articleRecommender "Michael Jackson" 10
Successfully inserted into the database.
["The Right Time (Hoodoo Gurus song)","Rols\248 Kapel","2023 IIHF World Championship final","Deme (disambiguation)","The Promise Man","Southern Drawl (album)","Roehampton House","Georgina Garc\237a P\233rez","Ion Theodorescu-Sion","FitzGerald dynasty"]
(1.90 secs, 800,520,512 bytes)
```
---

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

If you have any questions or suggestions, feel free to open an issue or contact me through the repository.



