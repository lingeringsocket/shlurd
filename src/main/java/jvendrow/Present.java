package jvendrow;

import java.util.HashMap;

public class Present extends Tense{
    public Present(String[] endingsA, String[] endingsE, String[] endingsI) {
        super(endingsA, endingsE, endingsI);
        putValuesGo();
    }

    //Conjugates verbs in the Present tense and handles irregular verbs
    public String conjugate(Conjugation conjugation) {
        String verb = conjugation.verb;
        int pn = conjugation.pn;

        String stemChange = stemChange(verb);
        String[] endings = endingsAEI(verb);
        //Checks of a verb is completely irregular
        if (iregsPresent.containsKey(verb)) {
            return print(conjugation, "", iregsPresent.get(verb), pn);
        }

        //Checks of a verb or any subsections of it have a change in the First Person
        else if(checkForIreg(verb, yoChange) >= 0) {
            int i = checkForIreg(verb, yoChange);
            return printYo(conjugation, verb.substring(0, i), verb.substring(i), stemChange, endings, pn);
        }

        //Checks of a verb ends with "cer" due to an exception
        else if(verb.substring(verb.length()-3).equals("cer") || verb.substring(verb.length()-3).equals("cir")) {
            return print(conjugation, verb.substring(0, verb.length()-3), new String[]{"zco", "ce", "ces", "cemos", "céis", "cen"}, pn);
        }

        //Checks of a verb ends with "uir" due to an exception
        else if(verb.substring(verb.length()-3).equals("uir")) {
            return print(conjugation, verb.substring(0, verb.length()-2) + "y", root(verb), endings, pn);
        }

        //Checks of a verb ends with "uir" due to an exception
        else if(verb.substring(verb.length()-3).equals("uar")) {
            return print(conjugation, verb.substring(0, verb.length()-3) + "ú", root(verb), endings, pn);
        }

        //Checks of a verb ends with "iar" due to an exception
        else if(verb.substring(verb.length()-3).equals("iar")) {
            return print(conjugation, verb.substring(0, verb.length()-3) + "í", root(verb), endings, pn);
        }

        else {
            return print(conjugation, root(stemChange), root(verb), endings, pn);
        }
    }

    protected HashMap<String, String> yoChange = new HashMap<String, String>();
    private HashMap<String, String[]> iregsPresent = new HashMap<String, String[]>();


    private void putValuesGo() {
        yoChange.put("hacer", "hago");
        yoChange.put("decir", "digo");
        yoChange.put("traer", "traigo");
        yoChange.put("salir", "salgo");
        yoChange.put("tener", "tengo");
        yoChange.put("caer", "caigo");
        yoChange.put("valer", "valgo");
        yoChange.put("venir", "vengo");
        yoChange.put("saber", "sé");
        yoChange.put("poner", "pongo");
        yoChange.put("dar", "doy");
        yoChange.put("caber", "caigo");
        yoChange.put("oir", "oigo");
        yoChange.put("gir", "jo");

        iregsPresent.put("haber", new String[] {"he", "has", "ha", "hemos", "habéis", "han"});
        iregsPresent.put("estar", new String[] {"estoy", "estás", "está", "estamos", "estáis", "están"});
        iregsPresent.put("ser", new String[] {"soy", "eres", "es", "somos", "sois", "son"});
        iregsPresent.put("ir", new String[] {"voy", "vas", "va", "vamos", "vías", "van"});
        iregsPresent.put("oir", new String[] {"oigo", "oyes", "oye", "oímos", "oís", "oyen"});
        iregsPresent.put("reir", new String[] {"rió", "ríes", "ríe", "reímos", "reís", "ríen"});

    }

    //Print method that handles stem changing verbs
    private String print(Conjugation conjugation, String root, String stemChange, String[] ends, int i) {
        if(i == 3 || i == 4) {
            return(conjugation.toBeReflexive[i] + stemChange + ends[i]);
        } else {
            return(conjugation.toBeReflexive[i] + root + ends[i]);
        }
    }

    //Print method that handles both irregular verbs in the first person and stem changes
    protected String printYo(Conjugation conjugation, String beginning, String verb, String withChange, String[] ends, int i) {
        if(i == 0) {
            return(conjugation.toBeReflexive[i] + beginning + yoChange.get(verb));
        } else if(i == 3 || i == 4) {
            return(conjugation.toBeReflexive[i] + beginning + root(verb) + ends[i]);
        } else {
            return(conjugation.toBeReflexive[i] + root(withChange) + ends[i]);
        }
    }

}
