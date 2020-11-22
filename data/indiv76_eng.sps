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
  PROV  'PROVINCE'
  CMA  'CENSUS METROPOLITAN AREA'
  FTYPE  'FILE TYPE INDICATOR'
  HHLDCLAS  'HOUSEHOLD CLASSIFICATION'
  HHDLREL  'HOUSEHOLD STATUS'
  FAMMEMB  'FAMILY STATUS'
  FAMSIZE  'NUMBER OF PERSONS IN FAMILY'
  SEX  'SEX'
  USMARST  'MARITAL STATUS'
  USMOTHTG  'MOTHER TONGUE'
  MTMP  'MOTHER TONGUE OF HUSBAND/MALE LONE PARENT'
  MTFP  'MOTHER TONGUE OF WIFE OR FEMALE LONE PARENT'
  AGE  'AGE'
  EDUCAT  'LEVEL OF SCHOOLING'
  EDUMP  'LEVEL OF SCHOOLING OF HUSBAND OR MALE LONE PARENT'
  EDUFP  'LEVEL OF SCHOOLING OF WIFE OR FEMALE LONE PARENT'
  ATTEND  'SCHOOL ATTENDANCE STATUS'
  ATYPE  'TYPE OF SCHOOL ATTENDANCE'
  DGREE  'EDUCATIONAL QUALIFICATIONS'
  PSUV  'POST-SECONDARY UNIVERSITY'
  PSNU  'POST-SECONDARY NON-UNIVERSITY'
  HGRAD  'HIGHEST GRADE ATTENDED'
  LFTAG  'LABOUR FORCE ACTIVITY (LAST WEEK)'
  LF71X  'LABOUR FORCE ACTIVITY (1971 DEFINITION)'
  MOB5  'MOBILITY STATUS'
  POP5  'POPULATION SIZE GROUP, 1971'
  POP  'POPULATION SIZE GROUP, 1976'
  RUSIZES  'RURAL/URBAN, 1976'
  RUUB5  'RURAL/URBAN, 1971'.
VALUE LABELS
    PROV   10  'NEWFOUNDLAND'
          12  'NOVA SCOTIA'
          13  'NEW BRUNSWICK'
          24  'QUEBEC'
          35  'ONTARIO'
          46  'MANITOBA'
          47  'SASKATCHEWAN'
          48  'ALBERTA'
          59  'BRITISH COLUMBIA'
         /
    CMA   462  'MONTREAL CMA'
          535  'TORONTO CMA'
          933  'VANCOUVER CMA'
          0  'NOT APPLICABLE'
         /
    FTYPE   1  'INDIVIDUAL FILE'
          2  'FAMILY FILE'
          3  'HOUSEHOLD FILE'
         /
    HHLDCLAS   1  'PRIVATE HOUSEHOLD'
          2  'COLLECTIVE HOUSEHOLD'
          3  'OTHER'
         /
    HHDLREL   1  'HEAD OF HOUSEHOLD'
          2  'SPOUSE'
          3  'SON OR DAUGHTER'
          4  'SON/DAUGHTER IN-LAW'
          5  'GRAND-CHILD'
          6  'FATHER OR MOTHER'
          7  'BROTHER OR SISTER'
          8  'FATHER/MOTHER IN-LAW'
          9  'BROTHER/SISTER IN-LAW'
          10  'OTHER RELATIVE'
          11  'NON-RELATIVE'
          0  'NOT APPLICABLE'
         /
    FAMMEMB   1  'HUSBAND'
          2  'WIFE'
          3  'MALE LONE PARENT'
          4  'FEMALE LONE PARENT'
          5  'CHILD'
          6  'NON-FAMILY PERSON'
          0  'NOT APPLICABLE'
         /
    FAMSIZE   1  'NON-FAMILY PERSON'
          2  'TWO PERSONS'
          3  'THREE PERSONS'
          4  'FOUR PERSONS'
          5  'FIVE PERSONS'
          6  'SIX PERSONS'
          7  'SEVEN PERSONS'
          8  'EIGHT PERSONS'
          9  'NINE PERSONS'
          10  'TEN+ PERSONS'
          0  'NOT APPLICABLE'
         /
    SEX   1  'MALE'
          2  'FEMALE'
         /
    USMARST   1  'SINGLE-NEVER MARRIED'
          2  'MARRIED & COMMON LAW'
          3  'WIDOWED'
          4  'DIVORCED'
          5  'SEPARATED'
         /
    USMOTHTG   1  'ENGLISH'
          2  'FRENCH'
          3  'GERMAN'
          4  'ITALIAN'
          5  'NETHERLANDIC'
          6  'POLISH'
          7  'SCANDINAVIAN'
          8  'UKRANIAN'
          9  'NATIVE INDIAN'
          10  'NOT STATED'
          11  'ALL OTHER'
         /
    MTMP   1  'ENGLISH'
          2  'FRENCH'
          3  'GERMAN'
          4  'ITALIAN'
          5  'NETHERLANDIC'
          6  'POLISH'
          7  'SCANDINAVIAN'
          8  'UKRANIAN'
          9  'NATIVE INDIAN'
          10  'NOT STATED'
          11  'ALL OTHER'
          0  'NOT APPLICABLE'
         /
    MTFP   1  'ENGLISH'
          2  'FRENCH'
          3  'GERMAN'
          4  'ITALIAN'
          5  'NETHERLANDIC'
          6  'POLISH'
          7  'SCANDINAVIAN'
          8  'UKRANIAN'
          9  'NATIVE INDIAN'
          10  'NOT STATED'
          11  'ALL OTHER'
          0  'NOT APPLICABLE'
         /
    AGE   95  '95 YEARS OF AGE OR OVER'
         /
    EDUCAT   
          1  'NO SCHOOLING'
          2  'KINDERGARTEN TO GRADE 4'
          3  'GRADES 5-8'
          4  'GRADES 9-10'
          5  'GRADE 11-NO SECONDARY CERTIFICATE'
          6  'GRADE 11-WITH SECONDARY CERTIFICATE'
          7  'GRADE 12-NO SECONDARY CERTIFICATE'
          8  'GRADE 12-WITH SECONDARY CERTIFICATE'
          9  'GR. 13-NO SECONDARY CERTIFICATE'
          10  'GRADE 13-WITH SECONDARY CERTIFICATE'
          11  'POST-SECONDARY NON UNIVERSITY ONLY-NO CERTIFICATE'
          12  'POST-SECONDARY NON UNIVERSITY ONLY-WITH CERTIFICATE'
          13  'UNIVERSITY ONLY-NO POST-SECONDARY CERTIFICATE, DIPLOMA OR DEGREE'
          14  'UNIVERSITY ONLY-WITH POST-SECONDARY NON-UNIVERSITY OR UNIVERSITY CERTIFICATE OR DIPLOMA'
          15  'UNIVERSITY ONLY-BACHELOR'
          16  'UNIVERSITY ONLY-MEDICAL DEGREE'
          17  'UNIVERSITY ONLY-MASTER DEGREE'
          18  'UNIVERSITY ONLY-DOCTORATE DEGREE'
          19  'BOTH UNIVERSITY AND NON-UNIVERSITY - NO POST-SECONDARY CERTIFICATE, DIPLOMA OR DEGREE'
          20  'BOTH UNIVERSITY AND NON-UNIVERSITY - WITH NON-UNIVERSITY CERTIFICATE OR DIPLOMA'
          21  'BOTH UNIVERSITY AND NON-UNIVERSITY  - WITH UNIVERSITY CERTIFICATE OR DIPLOMA'
          22  'BOTH UNIVERSITY AND NON-UNIVERSITY - BACHELOR DEGREE'
          23  'BOTH UNIVERSITY AND NON-UNIVERSITY - MEDICAL DEGREE'
          24  'BOTH UNIVERSITY AND NON-UNIVERSITY - MASTER DEGREE'
          25  'BOTH UNIVERSITY AND NON-UNIVERSITY - DOCTORATE DEGREE'
          0  'NOT APPLICABLE'
         /
    EDUMP   
          1  'NO SCHOOLING'
          2  'KINDERGARTEN TO GRADE 4'
          3  'GRADES 5-8'
          4  'GRADES 9-10'
          5  'GRADE 11-NO SECONDARY CERTIFICATE'
          6  'GRADE 11-WITH SECONDARY CERTIFICATE'
          7  'GRADE 12-NO SECONDARY CERTIFICATE'
          8  'GRADE 12-WITH SECONDARY CERTIFICATE'
          9  'GR. 13-NO SECONDARY CERTIFICATE'
          10  'GRADE 13-WITH SECONDARY CERTIFICATE'
          11  'POST-SECONDARY NON UNIVERSITY ONLY-NO CERTIFICATE'
          12  'POST-SECONDARY NON UNIVERSITY ONLY-WITH CERTIFICATE'
          13  'UNIVERSITY ONLY-NO POST-SECONDARY CERTIFICATE, DIPLOMA OR DEGREE'
          14  'UNIVERSITY ONLY-WITH POST-SECONDARY NON-UNIVERSITY OR UNIVERSITY CERTIFICATE OR DIPLOMA'
          15  'UNIVERSITY ONLY-BACHELOR'
          16  'UNIVERSITY ONLY-MEDICAL DEGREE'
          17  'UNIVERSITY ONLY-MASTER DEGREE'
          18  'UNIVERSITY ONLY-DOCTORATE DEGREE'
          19  'BOTH UNIVERSITY AND NON-UNIVERSITY - NO POST-SECONDARY CERTIFICATE, DIPLOMA OR DEGREE'
          20  'BOTH UNIVERSITY AND NON-UNIVERSITY - WITH NON-UNIVERSITY CERTIFICATE OR DIPLOMA'
          21  'BOTH UNIVERSITY AND NON-UNIVERSITY  - WITH UNIVERSITY CERTIFICATE OR DIPLOMA'
          22  'BOTH UNIVERSITY AND NON-UNIVERSITY - BACHELOR DEGREE'
          23  'BOTH UNIVERSITY AND NON-UNIVERSITY - MEDICAL DEGREE'
          24  'BOTH UNIVERSITY AND NON-UNIVERSITY - MASTER DEGREE'
          25  'BOTH UNIVERSITY AND NON-UNIVERSITY - DOCTORATE DEGREE'
          0  'NOT APPLICABLE'
         /
    EDUFP   
          1  'NO SCHOOLING'
          2  'KINDERGARTEN TO GRADE 4'
          3  'GRADES 5-8'
          4  'GRADES 9-10'
          5  'GRADE 11-NO SECONDARY CERTIFICATE'
          6  'GRADE 11-WITH SECONDARY CERTIFICATE'
          7  'GRADE 12-NO SECONDARY CERTIFICATE'
          8  'GRADE 12-WITH SECONDARY CERTIFICATE'
          9  'GR. 13-NO SECONDARY CERTIFICATE'
          10  'GRADE 13-WITH SECONDARY CERTIFICATE'
          11  'POST-SECONDARY NON UNIVERSITY ONLY-NO CERTIFICATE'
          12  'POST-SECONDARY NON UNIVERSITY ONLY-WITH CERTIFICATE'
          13  'UNIVERSITY ONLY-NO POST-SECONDARY CERTIFICATE, DIPLOMA OR DEGREE'
          14  'UNIVERSITY ONLY-WITH POST-SECONDARY NON-UNIVERSITY OR UNIVERSITY CERTIFICATE OR DIPLOMA'
          15  'UNIVERSITY ONLY-BACHELOR'
          16  'UNIVERSITY ONLY-MEDICAL DEGREE'
          17  'UNIVERSITY ONLY-MASTER DEGREE'
          18  'UNIVERSITY ONLY-DOCTORATE DEGREE'
          19  'BOTH UNIVERSITY AND NON-UNIVERSITY - NO POST-SECONDARY CERTIFICATE, DIPLOMA OR DEGREE'
          20  'BOTH UNIVERSITY AND NON-UNIVERSITY - WITH NON-UNIVERSITY CERTIFICATE OR DIPLOMA'
          21  'BOTH UNIVERSITY AND NON-UNIVERSITY  - WITH UNIVERSITY CERTIFICATE OR DIPLOMA'
          22  'BOTH UNIVERSITY AND NON-UNIVERSITY - BACHELOR DEGREE'
          23  'BOTH UNIVERSITY AND NON-UNIVERSITY - MEDICAL DEGREE'
          24  'BOTH UNIVERSITY AND NON-UNIVERSITY - MASTER DEGREE'
          25  'BOTH UNIVERSITY AND NON-UNIVERSITY - DOCTORATE DEGREE'
          0  'NOT APPLICABLE'
         /
    ATTEND   1  'ATTENDING FULL-TIME'
          2  'ATTENDING PART-TIME'
          3  'NOT ATTENDING SCHOOL'
          0  'NOT APPLICABLE'
         /
    ATYPE   1  'INVALID'
          2  'POST-SECONDARY NON-UNIVERSITY.'
          3  'UNIVERSITY'
          4  'BLANK'
          0  'NOT APPLICABLE'
         /
    DGREE   1  'NONE'
          2  'SECONDARY SCHOOL GRADUATION CERTIFICATE'
          3  'NON-UNIVERSITY CERTIFICATE OR DIPLOMA'
          4  'UNIVERSITY CERTIFICATE OR DIPLOMA BELOW BACHELOR LEVEL'
          5  'BACHELOR DEGREE'
          6  'DEGREE IN MEDECINE, DENTISTRY, VETERINARY MEDECINE'
          7  'MASTER DEGREE'
          8  'EARNED DOCTORATE'
          0  'NOT APPLICABLE'
         /
    PSUV   1  'NONE'
          2  'ONE YEAR OR LESS'
          3  'TWO YEARS'
          4  'THREE YEARS'
          5  'FOUR YEARS'
          6  'FIVE YEARS'
          7  'SIX YEARS OR MORE'
          0  'NOT APPLICABLE'
         /
    PSNU   1  'NONE'
          2  'ONE YEAR OR LESS'
          3  'TWO YEARS'
          4  'THREE YEARS OR MORE'
          0  'NOT APPLICABLE'
         /
    HGRAD   1  'GRADE ONE'
          2  'GRADE TWO'
          3  'GRADE THREE'
          4  'GRADE FOUR'
          5  'GRADE FIVE'
          6  'GRADE SIX'
          7  'GRADE SEVEN'
          8  'GRADE EIGHT'
          9  'GRADE NINE'
          10  'GRADE TEN'
          11  'GRADE ELEVEN'
          12  'GRADE TWELVE'
          13  'GRADE THIRTEEN'
          14  'KINDERGARTEN'
          15  'NO SCHOOLING'
          0  'NOT APPLICABLE'
         /
    LFTAG   1  'WORKED LAST WEEK FOR PAY OR IN OWN BUSINESS'
          2  'WORKED IN UNPAID FAMILY WORK'
          3  'WITH A JOB BUT NOT AT WORK'
          4  'ON TEMPORARY LAY-OFF'
          5  'WAITING TO START NEW JOB'
          6  'LOOKED FOR WORK (AND AVAILABLE)'
          7  'NOT IN THE LABOUR FORCE (LOOKED, NOT AVAILABLE)'
          8  'NOT IN THE LABOUR FORCE (OTHER)'
          0  'NOT APPLICABLE'
         /
    LF71X   1  'EMPLOYED - PAY'
          2  'EMPLOYED - NO PAY'
          3  'UNEMPLOYED - LOOKED'
          4  'UNEMPLOYED - LAY-OFF'
          5  'EMPLOYED - ABSENT'
          6  'NOT IN THE LABOUR FORCE - NON-INMATE'
          7  'NOT IN THE LABOUR FORCE - INMATE'
          0  'NOT APPLICABLE'
         /
    MOB5   1  'SAME DWELLING'
          2  'SAME CENSUS SUBDIVISION (CSD)'
          3  'DIFFERENT CSD, SAME CENSUS DIVISION (CD)'
          4  'DIFFERENT CD, SAME PROVINCE'
          5  'SAME PROVINCE, SUB-PROVINCIAL STATUS NOT STATED'
          6  'DIFFERENT PROVINCE'
          7  'PROVINCE OF RESIDENCE IN 1971 NOT STATED'
          8  'OUTSIDE CANADA'
          0  'NOT APPLICABLE'
         /
    POP5   1  '1 - 999'
          2  '1,000 - 1,999'
          3  '2,000 - 4,999'
          4  '5,000 - 9,999'
          5  '10,000 - 24,999'
          6  '25,000 - 49,999'
          7  '50,000 - 74,999'
          8  '75,000 - 99,999'
          9  '100,000 - 249,999'
          10  '250,000 - 499,999'
          11  '500,000 - 999,999'
          12  '1,000,000 PLUS'
          13  'RESIDENCE IN 1971 NOT STATED'
          0  'NOT APPLICABLE'
         /
    POP   1  '1 - 999'
          2  '1,000 - 1,999'
          3  '2,000 - 4,999'
          4  '5,000 - 9,999'
          5  '10,000 - 24,999'
          6  '25,000 - 49,999'
          7  '50,000 - 74,999'
          8  '75,000 - 99,999'
          9  '100,000 - 249,999'
          10  '250,000 - 499,999'
          11  '500,000 - 999,999'
          12  '1,000,000 PLUS'
         /
    RUSIZES   1  'URBAN 500,000+'
          2  '100,000-499,999'
          3  '30,000-99,999'
          4  '10,000-29,999'
          5  '5,000-9,999'
          6  '2,500-4,999'
          7  '1,000-2,499'
          8  'RURAL NON-FARM'
          9  'RURAL FARM'
         /
    RUUB5   1  'RURAL 1971'
          2  'URBAN 1971'
          3  'RESIDENCE IN 1971 NOT STATED'
          0  'NOT APPLICABLE'
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
SAVE OUTFILE='F:\Maxine\SPSS files that need to be figured out\Census\1976\indiv76eng_correcting.sav'.