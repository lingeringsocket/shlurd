package jvendrow;

import java.util.HashMap;
import java.util.HashSet;

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
            return print(conjugation, verb.substring(0, verb.length()-3) + "j", iregs.get("ucir"), pn);
        }

        //Checks for verbs that end in ir and are irregular for e to i and e to ie stem changes
        else if(end(verb) == 'i' && (contains(verb, eToI) || (checkForIreg(verb, eToIe) > -1))) {
            String stemChange = "";
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'e') {
                    stemChange = changeValue(verb, "i", i);
                    break;
                }
            }
            return print(conjugation, verb, stemChange, root(verb), endings, pn);
        }

        //Checks for verbs that end in ir and are irregular for o to ue stem changes
        else if(end(verb) == 'i' && (contains(verb, oToUe))) {
            String stemChange = "";
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'o') {
                    String subst = stemChangeU(verb, i);
                    stemChange = changeValue(verb, subst, i);
                }
            }
            return print(conjugation, verb, stemChange, root(verb), endings, pn);
        }

        //Handles verbs with have a vowel as the third-to-last character (except u)
        else if("aeio".contains(verb.substring(verb.length()-3, verb.length()-2)) && end(verb) != 'a'){
            return print(conjugation, root(verb), iregs.get("addY"), pn);
        }

        //Checks if the verb ends with "uir" due to certain exceptions
        else if("u".contains(verb.substring(verb.length()-3, verb.length()-2)) && (end(verb) != 'a') && ((verb.charAt(verb.length()-4) == 'g') || ((pn%3) != 0))){
            if (verb.indexOf("seguir") >= 0) {
                return print(conjugation, verb.substring(0, verb.length()-6), iregs.get("seguir"), pn);
            } else {
                return print(conjugation, root(verb), iregs.get("uir"), pn);
            }
        }
        //Checks if a verb ends with "guar" due to an exception
        else if(verb.substring(verb.length()-4).equals("guar")) {
            return print(conjugation, verb.substring(0, verb.length()-3), iregs.get("guar"), pn);
        }

        else {
            return print(conjugation, verb, verb, root(carGarZar(verb)), endings, pn);
        }
    }

    private HashMap<String, String> iregRoots = new HashMap<String, String>();
    private HashMap<String, String[]> iregs = new HashMap<String, String[]>();
    

    static void putValues(
        HashMap<String, String> iregRootsMap,
        HashMap<String, String[]> iregsMap)
    {
        iregRootsMap.put("poder", "pud");
        iregRootsMap.put("querer", "quis");
        iregRootsMap.put("poner", "pus");
        iregRootsMap.put("tener", "tuv");
        iregRootsMap.put("andar", "anduv");
        iregRootsMap.put("saber", "sup");
        iregRootsMap.put("venir", "vin");
        iregRootsMap.put("decir", "dij");
        iregRootsMap.put("traer", "traj");
        iregRootsMap.put("haber", "hub");
        iregRootsMap.put("caber", "cup");

        iregsMap.put("Ireg", new String[] {"e", "iste", "o", "imos", "isteis", "ieron"});
        iregsMap.put("ucir", new String[] {"e", "iste", "o", "imos", "isteis", "eron"});
        iregsMap.put("ir", new String[] {"fui", "fuiste", "fue", "fuimos", "fuisteis", "fueron"});
        iregsMap.put("ser", new String[] {"fui", "fuiste", "fue", "fuimos", "fuisteis", "fueron"});
        iregsMap.put("estar", new String[] {"estuve", "estuviste", "estuvo", "estuvimos", "estuvisteis", "estuvieron"});
        iregsMap.put("dar", new String[] {"di", "diste", "dio", "dimos", "disteis", "dieron"});
        iregsMap.put("ver", new String[] {"vi", "viste", "vio", "vimos", "visteis", "vieron"});
        iregsMap.put("satisfacer", new String[] {"satisfice", "satisficiste", "satisfizo", "satisficimos", "satisficisteis", "satisficieron"});
        iregsMap.put("hacer", new String[] {"hice", "hiciste", "hizo", "hicimos", "hicisteis", "hicieron"});
        iregsMap.put("deshacer", new String[] {"deshice", "deshiciste", "deshizo", "deshicimos", "deshicisteis", "deshicieron"});
        iregsMap.put("distinguir", new String[] {"distinguí", "distinguiste", "distinguió", "distinguimos", "distinguisteis", "distinguieron"});
        iregsMap.put("extinguir", new String[] {"extinguí", "extinguiste", "extinguió", "extinguimos", "extinguisteis", "extinguieron"});
        iregsMap.put("reir", new String[] {"reí", "reíste", "rio", "reímos", "reísteis", "rieron"});
        iregsMap.put("sonreir", new String[] {"sonreí", "sonreíste", "sonrió", "sonreímos", "sonreísteis", "sonrieron"});
        iregsMap.put("freir", new String[] {"freí", "freíste", "frió", "freímos", "freísteis", "frieron"});
        iregsMap.put("criar", new String[] {"crié", "criaste", "crio", "criamos", "criasteis", "criaron"});
        iregsMap.put("guiar", new String[] {"guie", "guiaste", "guio", "guiamos", "guiasteis", "guiaron"});
        iregsMap.put("gruñir", new String[] {"gruñí", "gruñiste", "gruñó", "gruñimos", "gruñisteis", "gruñeron"});
        iregsMap.put("reñir", new String[] {"reñí", "reñiste", "riñó", "reñimos", "reñisteis", "riñeron"});
        iregsMap.put("teñir", new String[] {"teñí", "teñiste", "tiñó", "teñimos", "teñisteis", "tiñeron"});
        iregsMap.put("tañer", new String[] {"tañí", "tañiste", "tañó", "tañimos", "tañisteis", "tañeron"});
        iregsMap.put("zambullir", new String[] {"zambullí", "zambulliste", "zambulló", "zambullimos", "zambullisteis", "zambulleron"});
        iregsMap.put("DecirTraer", new String[]{"e", "iste", "o", "imos", "isteis", "eron"});
        iregsMap.put("addY", new String[] {"í", "íste", "yó", "ímos", "ísteis", "yeron"});
        iregsMap.put("uir", new String[] {"í", "iste", "yó", "ímos", "isteis", "yeron"});
        iregsMap.put("seguir", new String[] {"seguí", "seguiste", "siguió", "seguimos", "seguisteis", "siguieron"});
        iregsMap.put("guar", new String[] {"üé", "uaste", "uó", "uamos", "uasteis", "uaron"});
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
