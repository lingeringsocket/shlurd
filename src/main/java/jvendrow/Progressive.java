package jvendrow;

public class Progressive {
    //Finds the gerund of a verb
    public static String gerund(String verb) {
        return Tense.fixOrthography(gerundImpl(verb));
    }

    static String gerundImpl(String verb) {
        if(verb.equals("ir")) {
            return "yendo";
        }
        else if(verb.equals("reír")) {
            return "riendo";
        }
        else if(verb.equals("sonreír")) {
            return "sonriendo";
        }
        else if(verb.equals("freír")) {
            return "friendo";
        }
        //Handles an exception for poder
        else if(verb.equals("poder")) {
            return "pudiendo";
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
                    break;
                }
            }
            if (verb.charAt(verb.length()-3) == 'ñ') {
                return verb.substring(0, verb.length()-2) + "endo";
            } else {
                return verb.substring(0, verb.length() - 2) + "iendo";
            }

            //Finds the gerund for verbs that end with ir and are stem changing	 for o to ue
        }
        else if (verb.charAt(verb.length()-3) == 'ñ') {
            return verb.substring(0, verb.length()-2) + "endo";
        }
        else if(Tense.end(verb) == 'i' && (Tense.checkForIreg(verb, Tense.oToUe) >= 0)) {

            //Finds the first o from the end to replace with u
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'o') {
                    String subst = Tense.stemChangeU(verb, i);
                    verb = Tense.changeValue(verb, subst, i);
                }
            }
            return verb.substring(0, verb.length() - 2) + "iendo";
        }

        //Handles verbs with have a vowel as the third-to-last character
        else if("aeiou".contains(verb.substring(verb.length()-3, verb.length()-2)) && (Tense.end(verb) != 'a') && !((verb.length() > 6) && verb.substring(verb.length()-6).equals("inguir"))){
            return verb.substring(0, verb.length() - 2) + "yendo";
        }

        else {
            return verb.substring(0, verb.length() - 2) + "iendo";
        }
    }
}
