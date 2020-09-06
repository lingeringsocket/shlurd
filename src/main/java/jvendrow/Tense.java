package jvendrow;

import java.util.HashMap;

public abstract class Tense {
    protected String[] endings;
    protected String[] endingsA;
    protected String[] endingsE;
    protected String[] endingsI;
    protected String[] endingsEI;

    //Constructor
    public Tense(String[] endings) {
        this.endings = endings;
        addAccent();
    }

    //Constructor
    public Tense(String[] endingsA, String[] endingsE, String[] endingsI) {
        this.endingsA = endingsA;
        this.endingsE = endingsE;
        this.endingsI = endingsI;
        addAccent();
    }

    //Arrays of the various stem-changing verbs
    protected static String[] eToIe = {"pensar", "empezar", "comenzar", "preferir", "acertar", "tener", "venir", "cerrar", "mentir", "fregar", "hervir", "confesar", "defender", "negar", "sentir", "querer"};
    protected static String[] eToI = {"pedir", "decir", "seguir", "servir", "competir", "elegir", "cerregir", "vestir", "freír", "gemir", "repetir"};
    protected static String[] oToUe = {"dormir", "almorzar", "morir", "probar", "mover", "colgar", "mostrar", "contar", "costar", "recordar", "volver", "resolver", "solver", "jugar"};

    static String[] reflexive = {"me ", "te ", "se ", "nos ", "os ", "se "};
    static String[] toBeReflexive = {"", "", "", "", "", ""};
    static String[] noEndings = {};
    
    abstract public String conjugate(Conjugation conjugation);

    //Returns all but the first two characters in the string
    static String root(String verb) {
        return verb.substring(0, verb.length()-2);
    }

    //Returns the second to last character of a verb, which is either a, e, or i
    static char end(String verb) {
        return verb.charAt(verb.length()-2);
    }

    //Returns the last two characters in a verb, which are either ar, er, or ir
    static String ending(String verb) {
        return verb.substring(verb.length()-2);
    }

    //Basic print method
    static String print(Conjugation conjugation, String root, String[] ends, int i) {
        return(conjugation.toBeReflexive[i] + root + ends[i]);
    }

    //Change a value at a specific location to a new value
    static String changeValue(String verb, String newValue, int i) {
        return verb.substring(0, i) + newValue + verb.substring(i+1);
    }

    //Returns the endings of the verb
    String[] endingsAEI(String verb) {
        if(end(verb) == 'a') {
            return endingsA;
        } else if(end(verb) == 'e') {
            return endingsE;
        } else {
            return endingsI;
        }
    }

    //Updates the verb to account for stem changes
    String stemChange(String verb) {
        if(checkForIreg(verb, eToI) >= 0) {
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'e') {
                    return changeValue(verb, "i", i);
                }
            }
        } else if(checkForIreg(verb, eToIe) >= 0) {
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'e') {
                    return changeValue(verb, "ie", i);
                }
            }
        } else if(checkForIreg(verb, oToUe) >= 0) {
            for(int i = verb.length()-3; i >= 0; i--) {
                if(verb.charAt(i) == 'o' || (verb.charAt(i) == 'u' && verb.equals("jugar"))) {
                    return changeValue(verb, "ue", i);
                }
            }
        }
        return verb;
    }

    //Checks if the verb or any subsections of it are keys in a HashMap where the irregular "sub-verb" beings
    static int checkForIreg(String verb, HashMap<String, String> map) {
        for(int i = 0; i < verb.length(); i++) {
            if(map.containsKey(verb.substring(i))) {
                return i;
            }
        }
        return -1;
    }

    //Checks if the verb or any subsections of it are in a String array where the irregular "sub-verb" beings
    static int checkForIreg(String verb, String[] list) {
        for(int i = 0; i < verb.length(); i++) {
            if(contains(verb.substring(i), list)) {
                return i;
            }
        }
        return -1;
    }

    //Checks if a String array contains a value
    static boolean contains(String verb, String[] list) {
        for (int i = 0; i < list.length; i++) {
            if (list[i].equals(verb)) {
                return true;
            }
        }
        return false;
    }

    //Checks of a verbs ends with "car", "gar", or "zar" due to certain exceptions
    String carGarZar(String verb) {
        if(verb.length() < 3) {
            return verb;
        }
        String last3 = verb.substring(verb.length()-3);
        String root;
        if(last3.equals("car")) {
            root = verb.substring(0, verb.length()-3) + "qu";
        } else if(last3.equals("gar")) {
            root = verb.substring(0, verb.length()-3) + "gu";
        } else if(last3.equals("zar")) {
            root = verb.substring(0, verb.length()-3) + "c";
        } else {
            root = root(verb);
        }
        return root + ending(verb);

    }

    //Adds an accent to the first vowel
    String endReflexive(Conjugation conjugation, String root) {
        if(conjugation.toBeReflexive[0].equals("")) {
            return root;
        }
        for(int i = 1; i < root.length(); i++) {
            if("eiaou".contains(root.substring(root.length()-i, root.length()-i+1))) {
                return root.substring(0, root.length()-i) + accents.get(root.charAt(root.length()-i)) + root.substring(root.length()-i+1, root.length());
            }
        }
        return root;
    }

    private HashMap<Character, String> accents = new HashMap<Character, String>();

    //Fills a HashMap of letters and their corresponding accents
    private void addAccent() {
        accents.put('a', "á");
        accents.put('e', "é");
        accents.put('i', "í");
        accents.put('o', "ó");
        accents.put('u', "ú");
    }

    static class Conjugation
    {
        String verb;
        int pn;
        String original;
        boolean isReflexive;
        String [] toBeReflexive;
    }
}
