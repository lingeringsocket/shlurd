package jvendrow;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Arrays;

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
    protected static String[] eToIe = {
        "afreñir",
        "astreñir",
        "constreñir",
        "pensar",
        "empezar",
        "comenzar",
        "preferir",
        "acertar",
        "tener",
        "venir",
        "cerrar",
        "mentir",
        "fregar",
        "hervir",
        "confesar",
        "defender",
        "abnegar",
        "negar",
        "querer",
        "advertir",
        "alentar",
        "apretar",
        "arrepentir",
        "atender",
        "atravesar",
        "convertir",
        "descender",
        "despertar",
        "divertir",
        "encender",
        "entender",
        "extender",
        "gobernar",
        "helar",
        "herir",
        "invertir",
        "merendar",
        "nevar",
        "perder",
        "aliquebrar",
        "quebrar",
        "recomendar",
        "regar",
        "requerir",
        "sentir",
        "presentir",
        "consentir",
        "sentar",
        "sugerir",
        "tropezar",
        "abeldar",
        "calentar",
        "ablentar",
        "adestrar",
        "adormentar",
        "albeldar",
        "alebrar",
        "amelar",
        "aneblar",
        "apacentar",
        "apernar",
        "ascender",
        "asentar",
        "aserrar",
        "asosegar",
        "aspaventar",
        "aventar",
        "beldar",
        "cegar",
        "cerner",
        "cernir",
        "cimentar",
        "concernir",
        "concertar",
        "contender",
        "dentar",
        "discernir",
        "distender",
        "ferrar",
        "hacendar",
        "heder",
        "hender",
        "hendir",
        "herbar",
        "herrar",
        "herver",
        "incensar",
        "infernar",
        "inhestar",
        "jamerdar",
        "manifestar",
        "melar",
        "mentar",
        "plegar",
        "salpimentar",
        "sarmentar",
        "segar",
        "sembrar",
        "serrar",
        "sorregar",
        "sosegar",
        "subarrendar",
        "subtender",
        "temblar",
        "tender",
        "tentar",
        "transcender",
        "transfregar",
        "trascender",
        "trasegar",
        "trasfregar",
        "trasverter",
        "travesar",
        "ventar",
        "verter"
    };
    protected static String[] eToI = {
        "ceñir",
        "pedir",
        "decir",
        "seguir",
        "servir",
        "competir",
        "elegir",
        "corregir",
        "vestir",
        "freír",
        "gemir",
        "repetir",
        "derretir",
        "despedir",
        "medir",
        "regir",
        "reñir",
        "teñir",
        "acomedir",
        "adherir",
        "arrecir",
        "asentir",
        "circunferir",
        "colegir",
        "comedir",
        "concebir",
        "conferir",
        "controvertir",
        "deferir",
        "diferir",
        "digerir",
        "disentir",
        "envestir",
        "henchir",
        "heñir",
        "impedir",
        "inferir",
        "ingerir",
        "injerir",
        "inserir",
        "investir",
        "malherir",
        "manferir",
        "proferir",
        "rendir",
        "subvertir",
        "transferir",
        "trasferir",
        "travestir",
        "zaherir"
    };
    protected static String[] oToUe = {
        "dormir",
        "almorzar",
        "morir",
        "probar",
        "mover",
        "colgar",
        "amostrar",
        "mostrar",
        "demostrar",
        "contar",
        "acostar",
        "costar",
        "recostar",
        "recordar",
        "volver",
        "resolver",
        "solver",
        "jugar",
        "poder",
        "acordar",
        "agorar",
        "apostar",
        "doler",
        "encontrar",
        "llover",
        "renovar",
        "rogar",
        "soler",
        "asonar",
        "sonar",
        "disonar",
        "grandisonar",
        "resonar",
        "soñar",
        "torcer",
        "destorcer",
        "retorcer",
        "volar",
        "abuñolar",
        "aclocar",
        "acornar",
        "afollar",
        "amoblar",
        "amolar",
        "atronar",
        "azolar",
        "clocar",
        "cocer",
        "recocer",
        "descocer",
        "dedolar",
        "demoler",
        "denostar",
        "desamoblar",
        "descocer",
        "descollar",
        "descordar",
        "descornar",
        "desencordar",
        "desengrosar",
        "desflocar",
        "desforzar",
        "desmajolar",
        "desolar",
        "desoldar",
        "desollar",
        "despoblar",
        "destrocar",
        "discordar",
        "dolar",
        "emporcar",
        "enclocar",
        "encorar",
        "encordar",
        "engorar",
        "enllocar",
        "enrodar",
        "entortar",
        "escolar",
        "esforzar",
        "forzar",
        "holgar",
        "hollar",
        "mancornar",
        "morder",
        "poblar",
        "recocer",
        "recolar",
        "reforzar",
        "rehollar",
        "remoler",
        "remorder",
        "repoblar",
        "rescontrar",
        "resollar",
        "retostar",
        "retronar",
        "revolcar",
        "rodar",
        "solar",
        "soldar",
        "soltar",
        "subsolar",
        "superpoblar",
        "tostar",
        "trascolar",
        "trascordar",
        "trastrocar",
        "trocar",
        "tronar",
        "volcar"
    };

    static String[] reflexive = {"me ", "te ", "se ", "nos ", "os ", "se "};
    static String[] toBeReflexive = {"", "", "", "", "", ""};
    static String[] noEndings = {};

    static HashSet<String> iarAccented = new HashSet<>(Arrays.asList(
            "criar",
            "confiar",
            "enviar",
            "esquiar",
            "guiar",
            "vaciar",
            "variar",
            "desvariar",
            "inventariar",
            "pipiar",
            "rociar",
            "acuantiar",
            "adiar",
            "agriar",
            "aliar",
            "almadiar",
            "amnistiar",
            "ampliar",
            "ansiar",
            "arriar",
            "ataviar",
            "autografiar",
            "averiar",
            "aviar",
            "biografiar",
            "cablegrafiar",
            "calcografiar",
            "caligrafiar",
            "calofriar",
            "calosfriar",
            "cariar",
            "cartografiar",
            "chirriar",
            "ciar",
            "cinematografiar",
            "contrariar",
            "coreografiar",
            "cromolitografiar",
            "cuantiar",
            "cuchichiar",
            "dactilografiar",
            "demasiar",
            "desafiar",
            "desataviar",
            "desaviar",
            "descarriar",
            "desconfiar",
            "descriar",
            "desliar",
            "desviar",
            "enfriar",
            "engaliar",
            "enhastiar",
            "enlejiar",
            "enriar",
            "entrecriar",
            "escalofriar",
            "esgrafiar",
            "esperriar",
            "espiar",
            "estenografiar",
            "estriar",
            "expiar",
            "extasiar",
            "extraviar",
            "fiar",
            "fotografiar",
            "fotolitografiar",
            "gloriar",
            "hastiar",
            "jipiar",
            "liar",
            "litofotografiar",
            "litografiar",
            "malcriar",
            "mecanografiar",
            "miar",
            "mimeografiar",
            "paliar",
            "piar",
            "porfiar",
            "radiografiar",
            "radiotelegrafiar",
            "recriar",
            "reenviar",
            "refriar",
            "resfriar",
            "rujiar",
            "taquigrafiar",
            "telegrafiar",
            "triar",
            "vigiar",
            "xerografiar")
        );
    
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
        int limit = verb.length();
        if (verb.endsWith("dar") ||
            verb.endsWith("ver"))
        {
            limit = 1;
        }
        for(int i = 0; i < limit; i++) {
            if(map.containsKey(verb.substring(i))) {
                return i;
            }
        }
        return -1;
    }

    //Checks if the verb or any subsections of it are in a String array where the irregular "sub-verb" begins
    static int checkForIreg(String verb, String[] list) {
        int limit = verb.length();
        if (verb.endsWith("jugar") ||
            verb.endsWith("hender") ||
            verb.endsWith("alentar") ||
            verb.endsWith("plegar") ||
            verb.endsWith("negar") ||
            verb.endsWith("ferrar") ||
            verb.endsWith("mentar") ||
            verb.endsWith("dentar") ||
            verb.endsWith("tentar") ||
            verb.endsWith("cerrar") ||
            verb.endsWith("ventar") ||
            verb.endsWith("quebrar") ||
            verb.endsWith("tender") ||
            verb.endsWith("pensar") ||
            verb.endsWith("amelar") ||
            verb.endsWith("melar") ||
            verb.endsWith("helar") ||
            verb.endsWith("regar") ||
            verb.endsWith("sentar") ||
            verb.endsWith("sentir") ||
            verb.endsWith("rogar") ||
            verb.endsWith("sonar") ||
            verb.endsWith("colar") ||
            verb.endsWith("dolar") ||
            verb.endsWith("solar") ||
            verb.endsWith("soldar") ||
            verb.endsWith("hollar") ||
            verb.endsWith("ronar") ||
            verb.endsWith("forzar") ||
            verb.endsWith("torcer") ||
            verb.endsWith("costar") ||
            verb.endsWith("mostrar"))
        {
            limit = 1;
        }
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

    static boolean endsWithUar(String verb)
    {
        return verb.substring(verb.length()-3).equals("uar") && !"cg".contains(Character.toString(verb.charAt(verb.length()-4)));
    }

    static String fixOrthography(String s)
    {
        return s.replace("ñie", "ñe").replace("ñié", "ñé").replace("ñió", "ñó").
            replace("llie", "lle").replace("llié", "llé").replace("llió", "lló");
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
        return accentFirstVowel(root);
    }

    String accentFirstVowel(String root)
    {
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
