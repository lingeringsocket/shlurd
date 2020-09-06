package jvendrow;

import java.util.HashMap;

public class Preterite extends Tense{
    public Preterite(String[] endingsA, String[] endingsE, String[] endingsI) {
        super(endingsA, endingsE, endingsI);
        putValues(iregRoots, iregs);
    }

    //Conjugates verbs in the Preterite tense and handles irregular verbs
    public String conjugate(Conjugation conjugation) {
        String verb = conjugation.verb;
        int pn = conjugation.pn;
        String[] endings = endingsAEI(verb);

        //Handles certain irregulars that do not follow the same rules are the other irregular verbs
        if(iregs.containsKey(verb)) {
            return print(conjugation, "", iregs.get(verb), pn);
        }

        //Checks if a verb or any subsections of it are irregular
        else if(checkForIreg(verb, iregRoots) >= 0) {
            int i = checkForIreg(verb, iregRoots);

            //Checks if the irregular verb is decir or traer, which have a slightly different ending
            if(verb.substring(i).equals("decir") || (verb.substring(i).equals("traer"))) {
                return print(conjugation, verb.substring(0, i) + iregRoots.get(verb.substring(i)), iregs.get("DecirTraer"), pn);
            }

            else {
                return print(conjugation, verb.substring(0, i) + iregRoots.get(verb.substring(i)), iregs.get("Ireg"), pn);
            }
        }

        //Checks if the verb ends with "ucir" due to certain exceptions
        else if(verb.length() > 3 && verb.substring(verb.length()-4).equals("ucir")) {
            return print(conjugation, verb.substring(0, verb.length()-3) + "j", iregs.get("Ireg"), pn);
        }

        //Checks for verbs that end in ir and are irregular for e to i and e to ie stem changes
        else if(end(verb) == 'i' && (contains(verb, eToI) || contains(verb, eToIe))) {
            String stemChange = "";
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'e') {
                    stemChange = changeValue(verb, "i", i);
                }
            }
            return print(conjugation, verb, stemChange, root(verb), endings, pn);
        }

        //Checks for verbs that end in ir and are irregular for o to ue stem changes
        else if(end(verb) == 'i' && (contains(verb, oToUe))) {
            String stemChange = "";
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'o') {
                    stemChange = changeValue(verb, "u", i);
                }
            }
            return print(conjugation, verb, stemChange, root(verb), endings, pn);
        }

        //Handles verbs with have a vowel as the third-to-last character (except u)
        else if("aeio".contains(verb.substring(verb.length()-3, verb.length()-2)) && end(verb) != 'a'){
            return print(conjugation, root(verb), iregs.get("addY"), pn);
        }

        //Checks if the verb ends with "uir" due to certain exceptions
        else if("u".contains(verb.substring(verb.length()-3, verb.length()-2)) && end(verb) != 'a'){
            return print(conjugation, root(verb), iregs.get("uir"), pn);
        }

        else {
            return print(conjugation, verb, verb, root(carGarZar(verb)), endings, pn);
        }
    }

    private HashMap<String, String> iregRoots = new HashMap<String, String>();
    private HashMap<String, String[]> iregs = new HashMap<String, String[]>();

    static void putValues(HashMap<String, String> iregRootsMap, HashMap<String, String[]> iregsMap) {
        iregRootsMap.put("poder", "pud");
        iregRootsMap.put("querer", "quis");
        iregRootsMap.put("poner", "pus");
        iregRootsMap.put("hacer", "hic");
        iregRootsMap.put("tener", "tuv");
        iregRootsMap.put("andar", "anduv");
        iregRootsMap.put("saber", "sup");
        iregRootsMap.put("venir", "vin");
        iregRootsMap.put("decir", "dij");
        iregRootsMap.put("traer", "traj");
        iregRootsMap.put("haber", "hub");
        iregRootsMap.put("caber", "cup");

        iregsMap.put("Ireg", new String[] {"e", "iste", "o", "imos", "isteis", "ieron"});
        iregsMap.put("ir", new String[] {"fui", "fuiste", "fue", "fuimos", "fuisteis", "fueron"});
        iregsMap.put("ser", new String[] {"fui", "fuiste", "fue", "fuimos", "fuisteis", "fueron"});
        iregsMap.put("estar", new String[] {"estuve", "estuviste", "estuvo", "estuvimos", "estuvisteis", "estuvieron"});
        iregsMap.put("dar", new String[] {"di", "diste", "dio", "dimos", "disteis", "dieron"});
        iregsMap.put("ver", new String[] {"vi", "viste", "vio", "vimos", "visteis", "vieron"});
        iregsMap.put("reir", new String[] {"reí", "reíste", "roí", "reímos", "reísties", "rieron"});
        iregsMap.put("DecirTraer", new String[]{"e", "iste", "o", "imos", "isteis", "eron"});
        iregsMap.put("addY", new String[] {"í", "íste", "yó", "ímos", "ísteis", "yeron"});
        iregsMap.put("uir", new String[] {"í", "iste", "yó", "ímos", "isteis", "yeron"});

    }

    //Print method that handles verbs with changes in the first person and slight stem changes in the third person
    private String print(Conjugation conjugation, String verb, String withChange, String yoChange, String[] ends, int i) {
        if(i == 0) {
            return(conjugation.toBeReflexive[i] + yoChange + ends[i]);
        } else if(i == 2 || i == 5) {
            return(conjugation.toBeReflexive[i] + root(withChange) + ends[i]);
        } else {
            return(conjugation.toBeReflexive[i] + root(verb) + ends[i]);
        }
    }

}
