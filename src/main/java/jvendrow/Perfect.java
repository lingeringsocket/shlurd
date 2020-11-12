package jvendrow;

import java.util.HashMap;

public class Perfect {
    //Finds the verb's past participle
    public static String pastParticiple(String verb) {
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

    private static HashMap<String, String> participleIregs = new HashMap<String, String>();

    //Fills the HashMap of irregular past participles
    static {
        participleIregs.put("abrir", "abierto");
        participleIregs.put("jabrir", "jabrido");
        participleIregs.put("ejabrir", "ejabrido");
        participleIregs.put("cubrir", "cubierto");
        participleIregs.put("decir", "dicho");
        participleIregs.put("bendecir", "bendito");
        participleIregs.put("maldecir", "maldito");
        participleIregs.put("escribir", "escrito");
        participleIregs.put("inscribir", "inscrito");
        participleIregs.put("preinscribir", "preinscrito");
        participleIregs.put("proscribir", "proscrito");
        participleIregs.put("circunscribir", "circunscrito");
        participleIregs.put("adscribir", "adscrito");
        participleIregs.put("subscribir", "subscrito");
        participleIregs.put("suscribir", "suscrito");
        participleIregs.put("transcribir", "transcrito");
        participleIregs.put("trascribir", "trascrito");
        participleIregs.put("manuscribir", "manuscrito");
        participleIregs.put("freír", "frito");
        participleIregs.put("hacer", "hecho");
        participleIregs.put("satisfacer", "satisfecho");
        participleIregs.put("morir", "muerto");
        participleIregs.put("poner", "puesto");
        participleIregs.put("pudrir", "podrido");
        participleIregs.put("repudrir", "repodrido");
        participleIregs.put("resolver", "resuelto");
        participleIregs.put("romper", "roto");
        participleIregs.put("corromper", "corrompido");
        participleIregs.put("ver", "visto");
        participleIregs.put("prever", "previsto");
        participleIregs.put("volver", "vuelto");
        participleIregs.put("desvolver", "desvuelto");
        participleIregs.put("envolver", "envuelto");
        participleIregs.put("desenvolver", "desenvuelto");
        participleIregs.put("arrevolver", "arrevuelto");
        participleIregs.put("devolver", "devuelto");
        participleIregs.put("revolver", "revuelto");
        participleIregs.put("solver", "suelto");
        participleIregs.put("disolver", "disuelto");
        participleIregs.put("absolver", "absuelto");
    }
}
