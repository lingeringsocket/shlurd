package jvendrow;

import java.util.HashMap;

public class Future extends Tense {

    public Future(String[] endings) {
        super(endings);
        putValues();
    }

    //Conjugates verbs in the Future tense and handles irregular verbs
    public String conjugate(Conjugation conjugation) {
        String verb = conjugation.verb;
        int pn = conjugation.pn;

        //Checks if a verb of any subsections of it are irregular
        if(checkForIreg(verb, iregRoots) >= 0) {
            int i = checkForIreg(verb, iregRoots);
            return print(conjugation, verb.substring(0, i) + iregRoots.get(verb.substring(i)), endings, pn);
        }

        else {
            return print(conjugation, verb, endings, pn);
        }
    }

    private HashMap<String, String> iregRoots = new HashMap<String, String>();

    //Fills a mashmap of irregular verbs in the Future tense
    private void putValues() {
        iregRoots.put("poder", "podr");
        iregRoots.put("querer", "querr");
        iregRoots.put("poner", "pondr");
        iregRoots.put("hacer", "hice");
        iregRoots.put("tener", "tendr");
        iregRoots.put("caber", "cabr");
        iregRoots.put("saber", "sabr");
        iregRoots.put("venir", "vendr");
        iregRoots.put("decir", "dir");
        iregRoots.put("haber", "habr");
    }
}
