package jvendrow;

import java.util.HashMap;

public class Imperfect extends Tense{

    public Imperfect(String[] endingsA, String[] endingsE, String[] endingsI) {
        super(endingsA, endingsE, endingsI);
        putValues();
    }

    //Conjugates verbs in the Imperfect tense and handles irregular verbs
    public String conjugate(Conjugation conjugation) {
        String verb = conjugation.verb;
        int pn = conjugation.pn;
        String[] endings = endingsAEI(verb);

        //Checks if the verb is one of the three irregulars
        if(iregRoots.containsKey(verb)) {
            return print(conjugation, "", iregRoots.get(verb), pn);
        }

        else {
            return print(conjugation, root(verb), endings, pn);
        }
    }


    private HashMap<String, String[]> iregRoots = new HashMap<String, String[]>();

    //Fills a HashMap of irregular verbs in the Imperfect Tense
    private void putValues() {
        iregRoots.put("ver", new String[] {"veía", "veías", "veía", "veíamos", "veíais", "veían"});
        iregRoots.put("ser", new String[] {"era", "eras", "era", "éramos", "erais", "eran"});
        iregRoots.put("ir", new String[] {"iba", "ibas", "iba", "íbamos", "ibais", "iban"});
    }
}
