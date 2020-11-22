DATA LIST FILE='F:\Maxine\SPSS files that need to be figured out\Census\1976\indiv76.txt'/
    PROV      1 -    2
    CMA      3 -    5
    FTYPE      6 -    6
    HHLDCLAS      7 -    7
    HHDLREL      8 -    9
    FAMMEMB     10 -   10
    FAMSIZE     11 -   12
    SEX     13 -   13
    USMARST     14 -   14
    USMOTHTG     15 -   16
    MTMP     17 -   18
    MTFP     19 -   20
    AGE     21 -   22
    EDUCAT     23 -   24
    EDUMP     25 -   26
    EDUFP     27 -   28
    ATTEND     29 -   29
    ATYPE     30 -   30
    DGREE     31 -   31
    PSUV     32 -   32
    PSNU     33 -   33
    HGRAD     34 -   35
    LFTAG     36 -   36
    LF71X     37 -   37
    MOB5     38 -   38
    POP5     39 -   40
    POP     41 -   42
    RUSIZES     43 -   43
    RUUB5     44 -   44.
VARIABLE LABELS
  PROV  "CODE PROVINCIAL"
  CMA  "CODE DE RMR"
  FTYPE  "INDICATION DU GENRE DE FICHIER"
  HHLDCLAS  "CLASSIFICATION DES M�NAGES"
  HHDLREL  "SITUATION DES PARTICULIERS AU SEIN DES M�NAGES"
  FAMMEMB  "SITUATION DES PARTICULIERS AU SEIN DES FAMILLES"
  FAMSIZE  "NOMBRE DE PERSONNES DANS LA FAMILLE"
  SEX  "SEXE"
  USMARST  "�TAT MATRIMONIAL"
  USMOTHTG  "LANGUE MATERNELLE"
  MTMP  "LANGUE MATERNELLE DE L'�POUX OU DU PARENT SEUL DE SEXE MASCULIN"
  MTFP  "LANGUE MATERNELLE DE L'�POUSE OU DU PARENT SEUL DE SEXE F�MININ"
  AGE  "�GE"
  EDUCAT  "NIVEAU DE SCOLARIT�"
  EDUMP  "NIVEAU DE SCOLARIT� DE L'�POUX OU DU PARENT SEUL DE SEXE MASCULIN"
  EDUFP  "NIVEAU DE SCOLARIT� DE L'�POUSE OU DU PARENT SEUL DE SEXE F�MININ"
  ATTEND  "FR�QUENTATION SCOLAIRE"
  ATYPE  "GENRE DE FR�QUENTATION SCOLAIRE"
  DGREE  "DIPL�ME D'�DUCATION"
  PSUV  "�TUDES POSTSECONDAIRES UNIVERSITAIRES"
  PSNU  "�TUDES POSTSECONDAIRES NON UNIVERSITAIRES"
  HGRAD  "LA DERNI�RE ANN�E D'�TUDES PRIMAIRES OU SECONDAIRES"
  LFTAG  "ACTIVIT�"
  LF71X  "ACTIVIT� (D�FINITION DE 1971)"
  MOB5  "STATUT DE MOBILIT�"
  POP5  "GROUPE DE TAILLE DE LA POPULATION, 1971"
  POP  "GROUPE DE TAILLE DE LA POPULATION, 1976"
  RUSIZES  "RURAL/URBAIN, 1976"
  RUUB5  "RURAL/URBAIN, 1971".
VALUE LABELS
    PROV   10  "TERRE-NEUVE"
          12  "NOUVELLE-�COSSE"
          13  "NOUVEAU BRUNSWICK"
          24  "QU�BEC"
          35  "ONTARIO"
          46  "MANITOBA"
          47  "SASKATCHEWAN"
          48  "ALBERTA"
          59  "COLOMBIE-BRITANNIQUE"
         /
    CMA   462  "RMR DE MONTR�AL"
          535  "RMR DE TORONTO"
          933  "RMR DE VANCOUVER"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    FTYPE   1  "FICHIER DES PARTICULIERS"
          2  "FICHIER DES FAMILLES"
          3  "FICHIER DES M�NAGES"
         /
    HHLDCLAS   1  "M�NAGE PRIV�"
          2  "M�NAGE COLLECTIF"
          3  "AUTRE"
         /
    HHDLREL   1  "CHEF DE M�NAGE"
          2  "CONJOINT"
          3  "FILS, FILLE"
          4  "BRU, GENDRE"
          5  "PETIT-ENFANT"
          6  "P�RE OU M�RE"
          7  "FR�RE OU SOEUR"
          8  "BEAU-P�RE, BELLE-M�RE"
          9  "BEAU-FR�RE, BELLE-SOEUR"
          10  "AUTRES PERSONNES APPARENT�ES"
          11  "PERSONNES NON APPARENT�ES"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    FAMMEMB   1  "MEMBRE D'UNE FAMILLE DE RECENSEMENT: �POUX"
          2  "MEMBRE D'UNE FAMILLE DE RECENSEMENT: �POUSE"
          3  "MEMBRE D'UNE FAMILLE DE RECENSEMENT: PARENT SEUL DE SEXE MASCULIN"
          4  "MEMBRE D'UNE FAMILLE DE RECENSEMENT: PARENT SEUL DE SEXE F�MININ"
          5  "MEMBRE D'UNE FAMILLE DE RECENSEMENT: ENFANT"
          6  "PERSONNES HORS FAMILLE DE RECENSEMENT"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    FAMSIZE   1  "PERSONNES HORS FAMILLE"
          2  "PERSONNES FAISANT PARTIE D'UNE FAMILLE: DEUX PERSONNES"
          3  "PERSONNES FAISANT PARTIE D'UNE FAMILLE: TROIS PERSONNES"
          4  "PERSONNES FAISANT PARTIE D'UNE FAMILLE: QUATRE PERSONNES"
          5  "PERSONNES FAISANT PARTIE D'UNE FAMILLE: CINQ PERSONNES"
          6  "PERSONNES FAISANT PARTIE D'UNE FAMILLE: SIX PERSONNES"
          7  "PERSONNES FAISANT PARTIE D'UNE FAMILLE: SEPT PERSONNES"
          8  "PERSONNES FAISANT PARTIE D'UNE FAMILLE: HUIT PERSONNES"
          9  "PERSONNES FAISANT PARTIE D'UNE FAMILLE: NEUF PERSONNES"
          10  "PERSONNES FAISANT PARTIE D'UNE FAMILLE: DIX PERSONNES OU PLUS"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    SEX   1  "HOMME"
          2  "FEMME"
         /
    USMARST   1  "C�LIBATAIRE (JAMAIS MARI�(E))"
          2  "MARI�(E) (COMPREND LES PERSONNES VIVANT EN CONCUBINAGE)"
          3  "VEUF (VEUVE)"
          4  "DIVORC�(E)"
          5  "S�PAR�(E)"
         /
    USMOTHTG   1  "ANGLAIS"
          2  "FRAN�AIS"
          3  "ALLEMAND"
          4  "ITALIEN"
          5  "N�ERLANDIQUE"
          6  "POLONAIS"
          7  "SCANDINAVE"
          8  "UKRAINIEN"
          9  "INDIEN NORD-AM�RICAIN"
          10  "NON D�CLAR�E"
          11  "TOUTES LES AUTRES"
         /
    MTMP   1  "ANGLAIS"
          2  "FRAN�AIS"
          3  "ALLEMAND"
          4  "ITALIEN"
          5  "N�ERLANDIQUE"
          6  "POLONAIS"
          7  "SCANDINAVE"
          8  "UKRANIEN"
          9  "INDIEN NORD-AM�RICAIN"
          10  "NON D�CLAR�E"
          11  "TOUTES LES AUTRES"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    MTFP   1  "ANGLAIS"
          2  "FRAN�AIS"
          3  "ALLEMAND"
          4  "ITALIEN"
          5  "N�ERLANDIQUE"
          6  "POLONAIS"
          7  "SCANDINAVE"
          8  "UKRANIEN"
          9  "INDIEN NORD-AM�RICAIN"
          10  "NON D�CLAR�E"
          11  "TOUTES LES AUTRES"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    AGE   95  "TOUTE PERSONNE �G�E DE 95 ANS ET PLUS"
         /
    EDUCAT   
          1  "AUCUNE SCOLARIT�"
          2  "N'AYANT PAS ATTEINT LA 5I�MME ANN�E"
          3  "5I�MME � 8I�MME ANN�E"
          4  "9I�MME ET 10I�MME ANN�ES"
          5  "11I�MME ANN�E - SANS CERTIFICAT D'�TUDES SECONDAIRES"
          6  "11I�MME ANN�E - AVEC CERTIFICAT D'�TUDES SECONDAIRES"
          7  "12I�MME ANN�E - SANS CERTIFICAT D'�TUDES SECONDAIRES"
          8  "12I�MME ANN�E - AVEC CERTIFICAT D'�TUDES SECONDAIRES"
          9  "13I�MME ANN�E - SANS CERTIFICAT D'�TUDES SECONDAIRES"
          10  "13I�MME ANN�E - AVEC CERTIFICAT D'�TUDES SECONDAIRES"
          11  "�TUDES POSTSECONDAIRES NON UNIVERSITAIRES SEULEMENT - SANS CERTIFICAT"
          12  "�TUDES POSTSECONDAIRES NON UNIVERSITAIRES SEULEMENT - AVEC CERTIFICAT"
          13  "UNIVERSIT� SEULEMENT - SANS CERTIFICAT, DIPL�ME OU GRADE DE NIVEAU POSTSECONDAIRE"
          14  "UNIVERSIT� SEULEMENT - AVEC CERTIFICAT OU DIPL�ME POSTSECONDAIRE NON UNIVERSITAIRE/UNIVERSITAIRE"
          15  "UNIVERSIT� SEULEMENT - BACCALAUR�AT"
          16  "UNIVERSIT� SEULEMENT - GRADE EN M�DECINE"
          17  "UNIVERSIT� SEULEMENT - MA�TRISE"
          18  "UNIVERSIT� SEULEMENT - DOCTORAT"
          19  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - SANS CERTIFICAT, DIPL�ME OU GRADE DE NIVEAU POSTSECONDAIRE"
          20  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - AVEC CERTIFICAT OU DIPL�ME DE NIVEAU NON UNIVERSITAIRE"
          21  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - AVEC CERTIFICAT OU DIPL�ME DE NIVEAU UNIVERSITAIRE"
          22  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - BACCALAUR�AT"
          23  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - GRADE EN M�DECINE"
          24  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - MA�TRISE"
          25  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - DOCTORAT"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    EDUMP   
          1  "AUCUNE SCOLARIT�"
          2  "N'AYANT PAS ATTEINT LA 5I�MME ANN�E"
          3  "5I�MME � 8I�MME ANN�E"
          4  "9I�MME ET 10I�MME ANN�ES"
          5  "11I�MME ANN�E - SANS CERTIFICAT D'�TUDES SECONDAIRES"
          6  "11I�MME ANN�E - AVEC CERTIFICAT D'�TUDES SECONDAIRES"
          7  "12I�MME ANN�E - SANS CERTIFICAT D'�TUDES SECONDAIRES"
          8  "12I�MME ANN�E - AVEC CERTIFICAT D'�TUDES SECONDAIRES"
          9  "13I�MME ANN�E - SANS CERTIFICAT D'�TUDES SECONDAIRES"
          10  "13I�MME ANN�E - AVEC CERTIFICAT D'�TUDES SECONDAIRES"
          11  "�TUDES POSTSECONDAIRES NON UNIVERSITAIRES SEULEMENT - SANS CERTIFICAT"
          12  "�TUDES POSTSECONDAIRES NON UNIVERSITAIRES SEULEMENT - AVEC CERTIFICAT"
          13  "UNIVERSIT� SEULEMENT - SANS CERTIFICAT, DIPL�ME OU GRADE DE NIVEAU POSTSECONDAIRE"
          14  "UNIVERSIT� SEULEMENT - AVEC CERTIFICAT OU DIPL�ME POSTSECONDAIRE NON UNIVERSITAIRE/UNIVERSITAIRE"
          15  "UNIVERSIT� SEULEMENT - BACCALAUR�AT"
          16  "UNIVERSIT� SEULEMENT - GRADE EN M�DECINE"
          17  "UNIVERSIT� SEULEMENT - MA�TRISE"
          18  "UNIVERSIT� SEULEMENT - DOCTORAT"
          19  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - SANS CERTIFICAT, DIPL�ME OU GRADE DE NIVEAU POSTSECONDAIRE"
          20  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - AVEC CERTIFICAT OU DIPL�ME DE NIVEAU NON UNIVERSITAIRE"
          21  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - AVEC CERTIFICAT OU DIPL�ME DE NIVEAU UNIVERSITAIRE"
          22  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - BACCALAUR�AT"
          23  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - GRADE EN M�DECINE"
          24  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - MA�TRISE"
          25  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - DOCTORAT"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    EDUFP   
          1  "AUCUNE SCOLARIT�"
          2  "N'AYANT PAS ATTEINT LA 5I�MME ANN�E"
          3  "5I�MME � 8I�MME ANN�E"
          4  "9I�MME ET 10I�MME ANN�ES"
          5  "11I�MME ANN�E - SANS CERTIFICAT D'�TUDES SECONDAIRES"
          6  "11I�MME ANN�E - AVEC CERTIFICAT D'�TUDES SECONDAIRES"
          7  "12I�MME ANN�E - SANS CERTIFICAT D'�TUDES SECONDAIRES"
          8  "12I�MME ANN�E - AVEC CERTIFICAT D'�TUDES SECONDAIRES"
          9  "13I�MME ANN�E - SANS CERTIFICAT D'�TUDES SECONDAIRES"
          10  "13I�MME ANN�E - AVEC CERTIFICAT D'�TUDES SECONDAIRES"
          11  "�TUDES POSTSECONDAIRES NON UNIVERSITAIRES SEULEMENT - SANS CERTIFICAT"
          12  "�TUDES POSTSECONDAIRES NON UNIVERSITAIRES SEULEMENT - AVEC CERTIFICAT"
          13  "UNIVERSIT� SEULEMENT - SANS CERTIFICAT, DIPL�ME OU GRADE DE NIVEAU POSTSECONDAIRE"
          14  "UNIVERSIT� SEULEMENT - AVEC CERTIFICAT OU DIPL�ME POSTSECONDAIRE NON UNIVERSITAIRE/UNIVERSITAIRE"
          15  "UNIVERSIT� SEULEMENT - BACCALAUR�AT"
          16  "UNIVERSIT� SEULEMENT - GRADE EN M�DECINE"
          17  "UNIVERSIT� SEULEMENT - MA�TRISE"
          18  "UNIVERSIT� SEULEMENT - DOCTORAT"
          19  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - SANS CERTIFICAT, DIPL�ME OU GRADE DE NIVEAU POSTSECONDAIRE"
          20  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - AVEC CERTIFICAT OU DIPL�ME DE NIVEAU NON UNIVERSITAIRE"
          21  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - AVEC CERTIFICAT OU DIPL�ME DE NIVEAU UNIVERSITAIRE"
          22  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - BACCALAUR�AT"
          23  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - GRADE EN M�DECINE"
          24  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - MA�TRISE"
          25  "�TUDES UNIVERSITAIRES ET NON UNIVERSITAIRES - DOCTORAT"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    ATTEND   1  "FR�QUENTATION � PLEIN TEMPS"
          2  "FR�QUENTATION � TEMPS PARTIEL"
          3  "NE FR�QUENTANT PAS L'�COLE"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    ATYPE   1  "INVALIDE"
          2  "POSTSECONDAIRE NON UNIVERSITAIRE"
          3  "UNIVERSITAIRE"
          4  "BLANC"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    DGREE   1  "AUCUN"
          2  "CERTIFICAT DE FIN D'�TUDES SECONDAIRES"
          3  "CERTIFICAT OU DIPL�ME DE NICEAU NON UNIVERSITAIRE"
          4  "CERTIFICAT OU DIPL�ME DE NIVEAU UNIVERSITAIRE INF�RIEUR AU BACCALAUR�AT"
          5  "BACCALAUR�AT"
          6  "GRADE EN M�DECINE, EN ART DENTAIRE OU EN M�DECINE V�T�RINAIRE"
          7  "MA�TRISE"
          8  "DOCTORAT ACQUIS"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    PSUV   1  "AUCUNE"
          2  "UN AN OU MOINS"
          3  "DEUX ANS"
          4  "TROIS ANS"
          5  "QUATRE ANS"
          6  "CINQ ANS"
          7  "SIX ANS OU PLUS"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    PSNU   1  "AUCUNE"
          2  "UN AN OU MOINS"
          3  "DEUX ANS"
          4  "TROIS ANS OU PLUS"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    HGRAD   1  "1I�RE ANN�E"
          2  "2I�MME ANN�E"
          3  "3I�MME ANN�E"
          4  "4I�MME ANN�E"
          5  "5I�MME ANN�E"
          6  "6I�MME ANN�E"
          7  "7I�MME ANN�E"
          8  "8I�MME ANN�E"
          9  "9I�MME ANN�E"
          10  "10I�MME ANN�E"
          11  "11I�MME ANN�E"
          12  "12I�MME ANN�E"
          13  "13I�MME ANN�E"
          14  "MATERNELLE"
          15  "AUCUNE SCOLARIT�"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    LFTAG   1  "TRAVAILLAIENT LA SEMAINE DERNI�RE CONTRE R�MUN�RATION OU DANS LEUR PROPRE ENTREPRISE"
          2  "TRAVAILLAIENT SANS R�MUN�RATION DANS UNE ENTREPRISE FAMILIALE"
          3  "AVAIENT UN EMPLOI DONT ILS �TAIENT ABSENTS"
          4  "�TAIENT EN CONG�DIEMENT TEMPORAIRE"
          5  "S'ATTENDAIENT � COMMENCER UN NOUVEL EMPLOI"
          6  "CHERCHAIENT DU TRAVAIL ET �TAIENT PR�TS � TRAVAILLER"
          7  "INACTIFS (� LA RECHERCHE D'UN EMPLOI, PAS PR�TS � COMMENCER � TRAVAILLER)"
          8  "INACTIFS (AUTRES)"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    LF71X   1  "ACTIFS - R�MUN�R�S"
          2  "ACTIFS - NON R�MUN�R�S"
          3  "CH�MEURS - CHARCHANT DU TRAVAIL"
          4  "CH�MEURS - CONG�DI�S"
          5  "ACTIFS - ABSENTS DU TRAVAIL"
          6  "INACTIFS - NON-PENSIONNAIRES D'INSTITUTION"
          7  "INACTIFS - PENSIONNAIRES D'INSTITUTION"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    MOB5   1  "M�ME LOGEMENT"
          2  "M�ME SUBDIVISION DE RECENSEMENT"
          3  "SUBDIVISION DE RECENSEMENT DIFF�RENTE, M�ME DIVISION DE RECENSEMENT"
          4  "DIVISION DE RECENSEMENT DIFF�RENTE, M�ME PROVINCE"
          5  "m�ME PROVINCE, STATUT INFRAPROVINCIAL NON D�CLAR�"
          6  "PROVINCE DIFF�RENTE"
          7  "PROVINCE DE R�SIDENCE EN 1971 NON D�CLAR�E"
          8  "� L'EXT�RIEUR DU CANADA"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    POP5   1  "1 - 999"
          2  "1,000 - 1,999"
          3  "2,000 - 4,999"
          4  "5,000 - 9,999"
          5  "10,000 - 24,999"
          6  "25,000 - 49,999"
          7  "50,000 - 74,999"
          8  "75,000 - 99,999"
          9  "100,000 - 249,999"
          10  "250,000 - 499,999"
          11  "500,000 - 999,999"
          12  "1,000,000 ET PLUS"
          13  "R�SIDENCE EN 1971 NON D�CLAR�E"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /
    POP   1  "1 - 999"
          2  "1,000 - 1,999"
          3  "2,000 - 4,999"
          4  "5,000 - 9,999"
          5  "10,000 - 24,999"
          6  "25,000 - 49,999"
          7  "50,000 - 74,999"
          8  "75,000 - 99,999"
          9  "100,000 - 249,999"
          10  "250,000 - 499,999"
          11  "500,000 - 999,999"
          12  "1,000,000 ET PLUS"
         /
    RUSIZES   1  "URBAN 500,000+"
          2  "100,000-499,999"
          3  "30,000-99,999"
          4  "10,000-29,999"
          5  "5,000-9,999"
          6  "2,500-4,999"
          7  "1,000-2,499"
          8  "RURALE NON AGRICOLE"
          9  "RURALE AGRICOLE"
         /
    RUUB5   1  "RURALE 1971"
          2  "URBAINE 1971"
          3  "R�SIDENCE EN 1971 NON D�CLAR�E"
          0  "N'AYANT PAS LIEU DE FIGURER"
         /.
MISSING VALUES
    CMA (0)/   
    HHDLREL (0)/ 
    FAMMEMB (0)/
    FAMSIZE (0)/
    MTMP (0)/ 
    MTFP (0)/
    EDUCAT (0)/       
    EDUMP (0)/  
    EDUFP (0)/   
    ATTEND (0)/
    ATYPE (0,1)/
    DGREE (0)/
    PSUV (0)/
    PSNU (0)/
    HGRAD (0)/
    LFTAG (0)/
    LF71X (0)/
    MOB5 (0)/
    POP5 (0)/
    RUUB5 (0)/. 
SAVE OUTFILE='F:\Maxine\SPSS files that need to be figured out\Census\1976\indiv76fre_creating.sav'.