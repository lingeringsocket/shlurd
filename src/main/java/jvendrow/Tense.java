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
    protected static String[] eToIe = {"pensar", "empezar", "comenzar", "preferir", "acertar", "tener", "venir", "cerrar", "mentir", "fregar", "hervir", "confesar", "defender", "negar", "sentir", "querer", "advertir", "alentar", "apretar", "arrepentir", "atender", "atravesar", "convertir", "descender", "despertar", "divertir", "encender", "entender", "extender", "gobernar", "helar", "herir", "invertir", "merendar", "nevar", "perder", "quebrar", "recomendar", "regar", "requerir", "sentir", "sentar", "sugerir", "tropezar"};
    protected static String[] eToI = {"pedir", "decir", "seguir", "servir", "competir", "elegir", "corregir", "vestir", "freír", "gemir", "repetir", "derretir", "despedir", "medir", "regir", "reñir", "teñir"};
    protected static String[] oToUe = {"dormir", "almorzar", "morir", "probar", "mover", "colgar", "mostrar", "contar", "costar", "recordar", "volver", "resolver", "solver", "jugar", "poder", "acordar", "agorar", "apostar", "doler", "encontrar", "llover", "renovar", "rogar", "soler", "sonar", "soñar", "torcer", "volar"};

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

    static String stemChangeU(String verb, int i) 
    {
        char pre = verb.charAt(i-1);
        if (pre == 'g') {
            return "ü";
        } else {
            return "u";
        }
    }

    static String substZC(String verb)
    {
        String base = verb.substring(0, verb.length()-3);
        char pre = verb.charAt(verb.length()-4);
        if ((pre == 'n') || (pre == 'r')) {
            return base + "z";
        } else {
            return base + "zc";
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
                    String subst = stemChangeU(verb, i) + "e";
                    return changeValue(verb, subst, i);
                }
            }
        }
        return verb;
    }

    //Checks if the verb or any subsections of it are keys in a HashMap where the irregular "sub-verb" begins
    static int checkForIreg(String verb, HashMap<String, String> map) {
        int limit = (verb.endsWith("dar") || verb.endsWith("ver")) ? 1 : verb.length();
        for(int i = 0; i < limit; i++) {
            if(map.containsKey(verb.substring(i))) {
                return i;
            }
        }
        return -1;
    }

    //Checks if the verb or any subsections of it are in a String array where the irregular "sub-verb" begins
    static int checkForIreg(String verb, String[] list) {
        int limit = (verb.endsWith("jugar") || verb.endsWith("helar") || verb.endsWith("regar") || verb.endsWith("sentar")) ? 1 : verb.length();
        for(int i = 0; i < limit; i++) {
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

    static boolean endsWithCerCir(String verb)
    {
        return (verb.substring(verb.length()-3).equals("cer") || verb.substring(verb.length()-3).equals("cir"));
    }

    //Checks if a verbs ends with "car", "gar", or "zar" due to certain exceptions
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
