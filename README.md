# Receptų Valdymas

Šis domenas yra skirtas receptų valdymui. Jis leidžia vartotojams pridėti, pašalinti, peržiūrėti ir ieškoti receptų virtualioje receptų knygoje. Sistema palaiko sudėtinius receptus, receptai gali būti sudaryti iš kitų receptų. Tokiu būdu, kiekvienas receptas gali turėti sudėtinius sub-receptus arba ingredientus, o tai suteikia įdomių rekursinių galimybių.

## Pagrindiniai elementai ir operacijos

**Pagrindiniai elementai:**
- **Receptas**: Receptai gali turėti ingredientų arba nuorodas į kitus receptus (sub-receptus). 
- **Ingredientas**: Sudaro ingrediento pavadinimas, kiekis ir kalorijų vertė.
- **Sub-receptas**: Leidžia receptui būti sudėtiniam, įtraukiant kitus receptus kaip ingredientus.

**Pagrindinės operacijos:**
- **add**: Prideda naują receptą su ingredientais arba sub-receptu.
- **remove**: Pašalina receptą pagal jo pavadinimą.
- **list recipes**: Išvardija visus turimus receptus.
- **search**: Leidžia ieškoti receptų pagal pavadinimą arba ingredientą.
- **exit**: Užbaigia programą.

## Rekursijos pavyzdys

Sistema leidžia kurti sudėtinius receptus, kur kiekvienas receptas gali turėti ingredientų sąrašą, kuriame gali būti kitas receptas. Šis rekursyvus elementas leidžia susikurti receptą, kuris savyje talpina kitus receptus, tokiu būdu didinant programos lankstumą.

Pvz.:
1. Komanda `add Pyragas miltai 500 200 cukrus 100 400` prideda pagrindinį receptą su paprastais ingredientais.
2. Komanda `add Sluoksniuotas_Pyragas Pyragas kremas 200 400` prideda naują receptą, kuris susideda iš sub-recepto `Pyragas` ir kitu ingredientų. Čia naudojama rekursija, leidžianti sukurti sudėtingesnius receptus.

## Naudojimo pavyzdžiai

- **Pridėti receptą su ingredientais**: `add Kepta_vištiena vištiena 200 450 aliejus 20 60`
- **Pridėti receptą su sub-receptu**: `add Vištiena_su_ryžiais Kepta_vištiena ryžiai 200 200`
- **Pašalinti receptą**: `remove Kepta_vištiena`
- **Ieškoti recepto pagal pavadinimą**: `search name Vištiena_su_ryžiais`
- **Ieškoti recepto pagal ingredientą**: `search ingredient ryžiai`
- **Peržiūrėti visus receptus**: `list recipes`

# Lab 2

- Ši programa leidžia naudotojams valdyti receptus per komandinę eilutę. Naudotojai gali pridėti, ieškoti, peržiūrėti ir pašalinti receptus.

## Programos naudojimas

- Norėdami paleisti programą naudokite `stack run fp2024-two`

## Naudojamos komandos

- add <Recipe> <subrecipes> <ingredients>
- remove <Recipe>
- list recipes
- search <Recipe> || <ingredient>
- exit

## Testavimas

- Testavimo sesija pateikta `lab2_example.txt` faile
  - Komandas, kurios buvo vykdomos programoje.
  - Programos atsakymus į pateiktas komandas.
 
## Automatinis testavimas

- Komandų parsinimas
- Programos būsenos pokyčiai
