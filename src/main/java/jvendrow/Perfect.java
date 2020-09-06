package jvendrow;

import java.util.HashMap;

public class Perfect extends Tense {
    private String type;
    
    private String[] present = {"he", "has", "ha", "hemos", "habéis",  "han"};
    private String[] imperfect = {"había", "habías", "había", "habíamos", "habíais", "habían"};
    private String[] future = {"habré", "habrás", "habrá", "habremos", "habréis", "habrán"};
    private String[] conditional = {"habría", "habrías", "habría", "habríamos", "habríais", "habrían"};
    private String[] subjunctive = {"haya", "hayas", "haya", "hayamos", "hayáis", "hayan"};
    private String[] pastSubjunctive = {"hubiera", "hubieras", "hubiera", "hubiéramos", "hubierais", "hubieran"};

    public Perfect(String type) {
        super(noEndings);
        this.type = type;
        fillParticiples();
    }

    //Conjugates verbs in the Perfect tenses and handles irregular verbs
    public String conjugate(Conjugation conjugation) {
        String verb = conjugation.verb;
        int pn = conjugation.pn;

        //Identifies the specific tense of the verb based on a parameter type
        if(type.equals("Present")) {
            return print(conjugation, present, verb, pn);
        }
        else if(type.equals("Imperfect")) {
            return print(conjugation, imperfect, verb, pn);
        }
        else if(type.equals("Future")) {
            return print(conjugation, future, verb, pn);
        }
        else if(type.equals("Subjunctive")){
            return print(conjugation, subjunctive, verb, pn);
        }
        else{
            return print(conjugation, pastSubjunctive, verb, pn);
        }
    }

    //Prints verbs in the Perfect tenses
    private String print(Conjugation conjugation, String[] haber, String verb, int i) {
        return(conjugation.toBeReflexive[i] + haber[i] + " " + pastParticiple(verb));
    }

    //Finds the verb's past participle
    private String pastParticiple(String verb) {
        if(Tense.checkForIreg(verb, participleIregs) >= 0) {
            int i = Tense.checkForIreg(verb, participleIregs);
            return verb.substring(0, i) + participleIregs.get(verb.substring(i));
        }else if(verb.charAt(verb.length()-2) == 'i' || verb.charAt(verb.length()-2) == 'e'|| verb.charAt(verb.length()-2) == 'í') {
            if (verb.length() > 2) {
                char i = verb.charAt(verb.length() - 3);
                if(i == 'a' || i == 'e' || i == 'o' || Tense.end(verb) == 'í') {
                    return verb.substring(0, verb.length() - 2) + "ído";
                } else {
                    return verb.substring(0, verb.length() - 2) + "ido";
                }
            }else {
                return verb.substring(0, verb.length() - 2) + "ido";
            }
        } else {
            return verb.substring(0, verb.length() - 2) + "ado";
        }
    }

    private HashMap<String, String> participleIregs = new HashMap<String, String>();

    //Fills the HashMap of irregular past participles
    private void fillParticiples() {
        participleIregs.put("abrir", "abierto");
        participleIregs.put("cubrir", "cubierto");
        participleIregs.put("decir", "dicho");
        participleIregs.put("escribir", "escrito");
        participleIregs.put("freír", "frito");
        participleIregs.put("hacer", "hecho");
        participleIregs.put("morir", "muerto");
        participleIregs.put("poner", "puesto");
        participleIregs.put("resolver", "resuelto");
        participleIregs.put("romper", "roto");
        participleIregs.put("ver", "visto");
        participleIregs.put("volver", "vuelto");
        participleIregs.put("solver", "suelto");
    }
}
