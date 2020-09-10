package jvendrow;

public class Progressive extends Tense {
    private String type;
    private String[] present = {"estoy", "estás", "está", "estamos", "estáis",  "están"};
    private String[] imperfect = {"estaba", "estabas", "estaba", "estábamos", "estabais", "estaban"};
    private String[] future = {"estaré", "estarás", "estará", "estaremos", "estaréis", "estarán"};

    public Progressive(String type) {
        super(noEndings);
        this.type = type;
    }

    //Conjugates verbs in the Progressive tenses and handles irregular verbs
    public String conjugate(Conjugation conjugation) {
        String verb = conjugation.verb;
        int pn = conjugation.pn;
        //Identified between the Present and Past progressive
        if(type.equals("Present")) {
            return print(conjugation, present, verb, pn);
        } else if(type.equals("Past")) {
            return print(conjugation, imperfect, verb, pn);
        } else if(type.equals("Future")) {
            return print(conjugation, future, verb, pn);
        } else {
            return "";
        }
    }

    //Print method for the progressive tense
    private String print(Conjugation conjugation, String[] estar, String verb, int i) {
        return(conjugation.toBeReflexive[i] + estar[i] + " " + gerund(verb));
    }

    //Finds the gerund of a verb
    static String gerund(String verb) {

        //Handles an exception for ir
        if(verb.equals("ir")) {
            return "yendo";
        }
        //Handles an exception for reír
        else if(verb.equals("reir")) {
            return "riendo";
        }

        //Find the gerund of verbs with an ar ending
        else if(Tense.end(verb) == 'a') {
            return verb.substring(0, verb.length() - 2) + "ando";

        }

        //Finds the gerund for verbs that end with ir and are stem changing for e to i or e to ie
        else if(Tense.end(verb) == 'i' && (Tense.checkForIreg(verb, Tense.eToI) >= 0 || Tense.checkForIreg(verb, Tense.eToIe) >= 0)) {

            //Finds the first e from the end to replace with i
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'e') {
                    verb = Tense.changeValue(verb, "i", i);
                }
            }
            return verb.substring(0, verb.length() - 2) + "iendo";

            //Finds the gerund for verbs that end with ir and are stem changing	 for o to ue
        } else if(Tense.end(verb) == 'i' && (Tense.checkForIreg(verb, Tense.oToUe) >= 0)) {

            //Finds the first o from the end ti replace with u
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'o') {
                    verb = Tense.changeValue(verb, "u", i);
                }
            }
            return verb.substring(0, verb.length() - 2) + "iendo";
        }

        //Handles verbs with have a vowel as the third-to-last character
        else if("aeiou".contains(verb.substring(verb.length()-3, verb.length()-2)) && Tense.end(verb) != 'a'){
            return verb.substring(0, verb.length() - 2) + "yendo";
        }

        else {
            return verb.substring(0, verb.length() - 2) + "iendo";
        }
    }
}
