package jvendrow;

import java.util.Arrays;
import java.util.HashMap;

public class Commands extends PresentSubjunctive{
    private String type;

    private static final String NO = "no ";

    public Commands(String[] endingsA, String[] endingsE, String[] endingsI, String type) {
        super(endingsA, endingsE, endingsI);
        this.type = type;
        fillMapCommands();
    }

    //Conjugates verbs in the Command tenses and handles irregular verbs
    public String conjugate(Conjugation conjugation) {
        String verb = conjugation.verb;
        int pn = conjugation.pn;
        String stemChange = stemChange(verb);
        String[] endings = endingsAEI(verb);

        //Checks if the verb is positive, reflexive, and irregular to handle reflexive irregulars
        if(type.equals("Positive") && posIregsReflexive.containsKey(verb) && !conjugation.toBeReflexive[0].equals("")) {
            return printIregs(conjugation, "", posIregsReflexive.get(verb), false, pn);
        }

        ////Checks if the verb is irregular and the Commands should be Positive
        else if(type.equals("Positive") && posIregs.containsKey(verb)) {
            return printIregs(conjugation, "", posIregs.get(verb), true, pn);
        }

        //Checks if the verb is irregular and the Commands should be Negative
        else if(type.equals("Negative") && iregs.containsKey(verb)) {
            return printIregs(conjugation, NO, Arrays.copyOfRange(iregs.get(verb), 1, 6), true, pn);
        }

        //Checks if a verb or any subsections of it have change matching that of the first person in the Present Tense
        else if((checkForIreg(verb, yoChange) >= 0) && !((pn < 2) && verb.contains("decir") && !verb.equals("decir"))) {
            int i = checkForIreg(verb, yoChange);
            return printYo(conjugation, verb.substring(0, i), verb.substring(i), yoChange.get(verb.substring(i)), endings, pn);
        }

        //Checks if the verb ends with "cer" or "cir" due to certain exceptions
        else if(endsWithCerCir(verb)) {
            String subst = substZC(verb);
            return print(conjugation, subst, subst, verb, subst, endings, pn);
        }
        //Checks if the verb ends with "uir" due to certain exceptions
        else if(verb.substring(verb.length()-3).equals("uir")) {
            return print(conjugation, verb.substring(0, verb.length()-2) + "y", verb, verb, verb.substring(0, verb.length()-2) + "y", endings, pn);
        }

        //Checks if the verb ends with "uar" due to certain exceptions
        else if(endsWithUar(verb)) {
            return print(conjugation, verb.substring(0, verb.length()-3) + "ú", verb, verb, root(verb), endings, pn);
        }

        //Checks if the verb ends with "iar" due to certain exceptions
        else if(iarAccented.contains(verb)) {
            return print(conjugation, verb.substring(0, verb.length()-3) + "í", verb, verb, root(verb), endings, pn);
        }

        else {
            //Uses the verb's gerund to handle certain stem changes
            String gerund = Progressive.gerund(verb);
            String withSmallChange = gerund.substring(0, gerund.length()-4);

            //Changes the amount removed from the verb bases on it's endings
            //Due to the difference in length between "ando" and "iendo"
            if(gerund.charAt(gerund.length()-4) != 'a') {
                withSmallChange = gerund.substring(0, gerund.length()-5);
            }

            return print(conjugation, root(carGarZar(stemChange)), root(carGarZar(verb)), verb, root(carGarZar(withSmallChange+ending(verb))), endings, pn);
        }
    }

    //Separates between positive and negative commands
    private String print(Conjugation conjugation, String stemChange, String verb, String originalVerb, String gerund, String[] ends, int pn) {
        if(type.equals("Positive")) {
            return printPositive(conjugation, stemChange, verb, originalVerb, gerund, ends, pn);
        } else {
            return printNegative(conjugation, stemChange, verb, gerund, ends, pn);
        }
    }
    //Separates between positive and negative commands for words with a change matching that of the Present tense first person
    protected String printYo(Conjugation conjugation, String beginning, String verb, String stemChange, String[] ends, int pn) {
        if(type.equals("Positive")) {
            return printYoPositive(conjugation, beginning, verb, stemChange, ends, pn);
        } else {
            return printYoNegative(conjugation, beginning, stemChange, ends, pn);
        }
    }

    //Prints positive commands with reflexive pronouns at the end of the verb and handles various slight changes
    private String printPositive(Conjugation conjugation, String stemChange, String verb, String originalVerb, String withSmallChange, String[] ends, int i) {
        if(i == 1) {
            //Checks if the ending of the verb is uir due to an exception

            if(originalVerb.substring(originalVerb.length()-3).equals("uir")) {
                return(conjugation.toBeReflexive[i] + root(stemChange(originalVerb)) + "y" + Endings.presentEr[2]);
            }
            //Checks if the ending of the verb is uar due to an exception
            else if(endsWithUar(originalVerb)) {
                return(conjugation.toBeReflexive[i] + stemChange(originalVerb).substring(0, originalVerb.length()-3) + "ú" + Endings.presentAr[2]);
            }
            else if (originalVerb.endsWith("guar")) {
                return(conjugation.toBeReflexive[i] + stemChange(originalVerb).substring(0, originalVerb.length()-2) + Endings.presentAr[2]);
            }
            //Checks if the ending of the verb is iar due to an exception
            else if(iarAccented.contains(originalVerb)) {
                return(conjugation.toBeReflexive[i] + stemChange(originalVerb).substring(0, originalVerb.length()-3) + "í" + Endings.presentAr[2]);
            }
            else if(end(originalVerb) == 'a') {
                return(endReflexive(conjugation, root(stemChange(originalVerb))) + Endings.presentAr[2] + conjugation.toBeReflexive[i]);
            }
            else {
                return(endReflexive(conjugation, root(stemChange(originalVerb))) + Endings.presentEr[2] + conjugation.toBeReflexive[i]);
            }
        }
        else if (((i == 2) || (i == 5)) && originalVerb.endsWith("guar")) {
            return(conjugation.toBeReflexive[i] + stemChange(originalVerb).substring(0, originalVerb.length()-3) + iregs.get("guar")[i]);
        }
        else if(i == 3) {
            //Handles exception for reflexive verbs for nosotros (we)
            if(!conjugation.toBeReflexive[0].equals("")) {
                if(ends[0].equals("e")) {
                    return(withSmallChange + "émonos");
                }
                else {
                    return(withSmallChange + "ámonos");
                }
            }
            else {
                return(withSmallChange + ends[i]);
            }
        }
        else if(i == 4) {
            if(!conjugation.toBeReflexive[0].equals("")) {
                return(conjugation.original.substring(0, conjugation.original.length()-2) + "íos");
            }
            else {
                return(conjugation.toBeReflexive[i] + endReflexive(conjugation, root(originalVerb)) + end(originalVerb) + "d");
            }
        }
        else {
            return(endReflexive(conjugation, stemChange) + ends[i] + conjugation.toBeReflexive[i]);
        }
    }

    //Prints negative commands and handles various slight changes
    private String printNegative(Conjugation conjugation, String stemChange, String verb, String withSmallChange, String[] ends, int i) {
        if(i == 0) {
            return "";
        } else if(i == 1) {
            return(NO + conjugation.toBeReflexive[i] + stemChange + ends[1]);
        } else if(i == 3 || i == 4) {
            return(NO + conjugation.toBeReflexive[i] + withSmallChange + ends[i]);
        } else {
            return(NO + conjugation.toBeReflexive[i] + stemChange + ends[i]);
        }
    }

    //Prints positive commands with a change matching the first person of the Present tense and handles various slight changes
    private String printYoPositive(Conjugation conjugation, String beginning, String verb, String stemChange, String[] ends, int i) {
        if(i == 1) {
            //Checks if the verb is irregular in the first person
            if(yoIreg.containsKey(verb)) {
                String base = yoIreg.get(verb);
                if (!beginning.isEmpty() && base.endsWith("n")) {
                    base = accentFirstVowel(base);
                }
                return(endReflexive(conjugation, beginning) + base + conjugation.toBeReflexive[i]);
            }
            else {
                if(end(verb) == 'a') {
                    return(endReflexive(conjugation, beginning + root(stemChange(verb))) + Endings.presentAr[2] + conjugation.toBeReflexive[i]);
                } else {
                    return(endReflexive(conjugation, beginning + root(stemChange(verb))) + Endings.presentEr[2] + conjugation.toBeReflexive[i]);
                }
            }
        }

        //Handles exception for reflexive verbs for nosotros (we)
        else if(i == 3 && !conjugation.toBeReflexive[0].equals("")) {
            if(ends[0].equals("e")) {
                return(beginning + stemChange.substring(0, stemChange.length()-1) + "émonos");
            }
            else {
                return(beginning + stemChange.substring(0, stemChange.length()-1) + "ámonos");
            }
        }
        else if(i == 4) {
            if(!conjugation.toBeReflexive[0].equals("")) {
                return(conjugation.original.substring(0, conjugation.original.length()-2) + "íos");
            }
            else {
                return(conjugation.toBeReflexive[i] + conjugation.original.substring(0, conjugation.original.length()-1) + "d");
            }
        } else {
            return(endReflexive(conjugation, beginning + stemChange.substring(0, stemChange.length()-1)) + ends[i] + conjugation.toBeReflexive[i]);
        }
    }

    //Prints negative commands with a change matching the first person of the Present tense and handles various slight changes
    private String printYoNegative(Conjugation conjugation, String beginning, String stemChange, String[] ends, int i) {
        if(i == 1) {
            return(NO + conjugation.toBeReflexive[i] + beginning + stemChange.substring(0, stemChange.length()-1) + ends[i]);
        }
        else {
            return(NO + conjugation.toBeReflexive[i] + beginning + stemChange.substring(0, stemChange.length()-1) + ends[i]);
        }
    }

    //Prints irregular verbs
    private String printIregs(Conjugation conjugation, String sense, String[] verbs, boolean putReflexive, int i) {
        if(putReflexive) {
            return(sense + conjugation.toBeReflexive[i] + verbs[i-1]);
        }
        else {
            return(verbs[i-1]);
        }
    }

    private HashMap<String, String[]> posIregs = new HashMap<String, String[]>();
    private HashMap<String, String[]> posIregsReflexive = new HashMap<String, String[]>();
    private HashMap<String, String> yoIreg = new HashMap<String, String>();


    //Fills the HashMaps for irregularities in the Commands
    private void fillMapCommands() {
        posIregs.put("ser", new String[]{"sé", "sea", "seamos", "sed", "sean"});
        posIregs.put("estar", new String[]{"está", "esté", "estemos", "estad", "estén"});
        posIregs.put("ir", new String[]{"ve", "vaya", "vamos", "id", "vayan"});
        posIregs.put("dar", new String[]{"da", "dé", "demos", "dad", "den"});
        posIregs.put("saber", new String[]{"sabe", "sepa", "sepamos", "sabed", "sepan"});
        posIregs.put("haber" , new String[]{"has", "haya", "hayamos", "habed", "hayan"});
        posIregs.put("reir" , new String[]{"ríe", "ría", "riamos", "reíd", "rían"});
        posIregs.put("sonreir" , new String[]{"sonríe", "sonría", "sonriamos", "sonreíd", "sonrían"});
        posIregs.put("freir" , new String[]{"fríe", "fría", "friamos", "freíd", "frían"});
        posIregs.put("errar" , new String[]{"yerra", "yerre", "yerramos", "errad", "yerren"});
        posIregs.put("gruñir", new String[] {"gruñe", "gruña", "gruñemos", "gruñed", "gruñan"});
        posIregs.put("oler", new String[] {"huele", "huela", "huelemos", "oled", "huelan"});
        posIregs.put("prever", new String[] {"prevé", "prevea", "prevemos", "preved", "prevean"});
        posIregs.put("ver", new String[] {"ve", "vea", "vemos", "ved", "vean"});
        posIregs.put("prohibir", new String[] {"prohíbe", "prohíba", "prohibemos", "prohibid", "prohíban"});
        posIregs.put("rehusar", new String[] {"rehúsa", "rehúse", "rehusamos", "rehusad", "rehúsen"});

        posIregsReflexive.put("dar", new String[]{"date", "dése", "démonos", "daos", "dense"});
        posIregsReflexive.put("ir" , new String[]{"vete", "váyase", "vayámonos", "idos", "váyanse"});
        posIregsReflexive.put("reir" , new String[]{"ríete", "ríase", "riámonos", "reíos", "ríanse"});

        yoIreg.put("tener", "ten");
        yoIreg.put("venir", "ven");
        yoIreg.put("poner", "pon");
        yoIreg.put("decir", "di");
        yoIreg.put("salir", "sal");
        yoIreg.put("hacer", "haz");
        yoIreg.put("satisfacer", "satisfaz");

        //Not an actual yo irregular, but has the same effect
        yoIreg.put("oir", "oye");
    }
}
