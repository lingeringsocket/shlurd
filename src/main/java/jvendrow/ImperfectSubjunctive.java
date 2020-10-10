package jvendrow;

import java.util.HashMap;

public class ImperfectSubjunctive extends Tense{

    private String[] irSer = {"fuera", "fueras", "fuera", "fuéramos", "fuerais", "fueran"};
    private String[] era = {"era", "eras", "era", "éramos", "erais", "eran"};

    private HashMap<String, String> iregRoots = new HashMap<String, String>();
    private HashMap<String, String[]> iregs = new HashMap<String, String[]>();

    public ImperfectSubjunctive(String[] endingsA, String[] endingsE, String[] endingsI) {
        super(endingsA, endingsE, endingsI);
        Preterite.putValues(iregRoots, iregs);
    }

    //Conjugates verbs in the Imperfect Subjunctive tense and handles irregular verbs
    public String conjugate(Conjugation conjugation) {
        String verb = conjugation.verb;
        int pn = conjugation.pn;

        String[] endings = endingsAEI(verb);

        //Checks for ir and ser, the most notable irregulars
        if(verb.equals("ir") || verb.equals("ser")) {
            return print(conjugation, "", irSer, pn);

        }
        //Handles dar, which uses a different ending due to its irregularity in the Preterite
        else if(verb.equals("dar")) {
            return print(conjugation, root(verb), endingsE, pn);
        }

        else if(verb.equals("reir")) {
            return print(conjugation, "r", endingsE, pn);
        }

        //Checks if a verb or any subsections of it are irregular
        else if(checkForIreg(verb, iregRoots) >= 0) {
            int i = checkForIreg(verb, iregRoots);

            //Checks if the verb is decir or traer, which have a slightly different ending
            if(verb.substring(i).equalsIgnoreCase("decir") || verb.substring(i).equalsIgnoreCase("traer")) {
                return print(conjugation, verb.substring(0, i) + iregRoots.get(verb.substring(i)), era, pn);
            } else {
                return print(conjugation, verb.substring(0, i) + iregRoots.get(verb.substring(i)), endings, pn);
            }
        }

        //Checks for verbs that end in ir and are irregular for e to i or e to ie stem changes
        else if(end(verb) == 'i' && (contains(verb, eToI) || contains(verb, eToIe))) {
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'e') {
                    return print(conjugation, root(changeValue(verb, "i", i)), endings, pn);
                }
            }
            return "";
        }

        //Checks for verbs that end in ir and are irregular for o to ue stem changes
        else if(end(verb) == 'i' && (contains(verb, oToUe))) {
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'o') {
                    String subst = stemChangeU(verb, i);
                    return print(conjugation, root(changeValue(verb, subst, i)), endings, pn);
                }
            }
            return "";
        }

        //Handles verbs with have a vowel as the third-to-last character (except u)
        else if("aeio".contains(verb.substring(verb.length()-3, verb.length()-2)) && end(verb) != 'a'){
            return print(conjugation, root(verb) + "y", era, pn);
        }

        //Checks if the verb ends with "uir" due to certain exceptions
        else if(verb.substring(verb.length()-3, verb.length()-1).equals("ui")) {
            return print(conjugation, verb.substring(0, verb.length()-2) + "y", era, pn);
        }

        //Checks if the verb ends with "ucir" due to certain exceptions
        else if(verb.length() > 3 && verb.substring(verb.length()-4).equals("ucir")) {
            return print(conjugation, verb.substring(0, verb.length()-3) + "j", era, pn);
        }

        else {
            return print(conjugation, root(verb), endings, pn);
        }
    }
}
