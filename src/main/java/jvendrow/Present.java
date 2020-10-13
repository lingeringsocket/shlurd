package jvendrow;

import java.util.HashMap;
import java.util.HashSet;

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
        //Checks if a verb is completely irregular
        if (iregsPresent.containsKey(verb)) {
            return print(conjugation, "", iregsPresent.get(verb), pn);
        }

        //Checks if a verb or any subsections of it have a change in the First Person
        else if(checkForIreg(verb, yoChange) >= 0) {
            int i = checkForIreg(verb, yoChange);
            return printYo(conjugation, verb.substring(0, i), verb.substring(i), stemChange, endings, pn);
        }

        //Checks if a verb ends with "cer" due to an exception
        else if(endsWithCerCir(verb)) {
            String subst = substZC(verb);
            return print(conjugation, verb.substring(0, verb.length()-3), new String[]{subst.substring(verb.length()-3) + "o", "ces", "ce", "cemos", "céis", "cen"}, pn);
        }

        //Checks if a verb ends with "uir" due to an exception
        else if(verb.substring(verb.length()-3).equals("uir")) {
            return print(conjugation, verb.substring(0, verb.length()-2) + "y", root(verb), endings, pn);
        }

        //Checks if a verb ends with "uar" due to an exception
        else if(endsWithUar(verb)) {
            return print(conjugation, verb.substring(0, verb.length()-3) + "ú", root(verb), endings, pn);
        }
        //Checks if a verb ends with "iar" due to an exception
        else if(iarAccented.contains(verb)) {
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
        yoChange.put("caer", "caigo");
        yoChange.put("caber", "quepo");
        yoChange.put("regir", "rijo");
        yoChange.put("elegir", "elijo");
        yoChange.put("tinguir", "tingo");
        yoChange.put("oir", "oigo");
        yoChange.put("gir", "jo");
        yoChange.put("ger", "jo");
        yoChange.put("seguir", "sigo");
        yoChange.put("torcer", "tuerzo");
        yoChange.put("satisfacer", "satisfago");

        iregsPresent.put("haber", new String[] {"he", "has", "ha", "hemos", "habéis", "han"});
        iregsPresent.put("oler", new String[] {"huelo", "hueles", "huele", "olemos", "oléis", "huelen"});
        iregsPresent.put("estar", new String[] {"estoy", "estás", "está", "estamos", "estáis", "están"});
        iregsPresent.put("errar", new String[] {"yerro", "yerras", "yerra", "erramos", "erráis", "yerran"});
        iregsPresent.put("ser", new String[] {"soy", "eres", "es", "somos", "sois", "son"});
        iregsPresent.put("prever", new String[] {"preveo", "prevés", "prevé", "prevemos", "prevéis", "prevén"});
        iregsPresent.put("ver", new String[] {"veo", "ves", "ve", "vemos", "veis", "ven"});
        iregsPresent.put("ir", new String[] {"voy", "vas", "va", "vamos", "vais", "van"});
        iregsPresent.put("dar", new String[] {"doy", "das", "da", "damos", "dais", "dan"});
        iregsPresent.put("oir", new String[] {"oigo", "oyes", "oye", "oímos", "oís", "oyen"});
        iregsPresent.put("reir", new String[] {"río", "ríes", "ríe", "reímos", "reís", "ríen"});
        iregsPresent.put("sonreir", new String[] {"sonrío", "sonríes", "sonríe", "sonreímos", "sonreís", "sonríen"});
        iregsPresent.put("freir", new String[] {"frío", "fríes", "fríe", "freímos", "freís", "fríen"});
        iregsPresent.put("prohibir", new String[] {"prohíbo", "prohíbes", "prohíbe", "prohibimos", "prohibís", "prohíben"});
        iregsPresent.put("rehusar", new String[] {"rehúso", "rehúsas", "rehúsa", "rehusamos", "rehusáis", "rehúsan"});

        // ought to use a generic "-ducir" rule instead
        iregsPresent.put("conducir", new String[] {"conduzco", "conduces", "conduce", "conducimos", "conducís", "conducen"});
        iregsPresent.put("inducir", new String[] {"induzco", "induces", "induce", "inducimos", "inducís", "inducen"});
        iregsPresent.put("introducir", new String[] {"introduzco", "introduces", "introduce", "introducimos", "introducís", "introducen"});
        iregsPresent.put("producir", new String[] {"produzco", "produces", "produce", "producimos", "producís", "producen"});
        iregsPresent.put("reducir", new String[] {"reduzco", "reduces", "reduce", "reducimos", "reducís", "reducen"});
        iregsPresent.put("traducir", new String[] {"traduzco", "traduces", "traduce", "traducimos", "traducís", "traducen"});
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
