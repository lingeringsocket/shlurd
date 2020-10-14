package jvendrow;

import java.util.HashMap;

public class PresentSubjunctive extends Present{

    public PresentSubjunctive(String[] endingsA, String[] endingsE, String[] endingsI) {
        super(endingsA, endingsE, endingsI);
        fillMap();
    }

    //Conjugates verbs in the Present Subjunctive tense and handles irregular verbs
    public String conjugate(Conjugation conjugation) {
        String verb = conjugation.verb;
        int pn = conjugation.pn;

        String stemChange = stemChange(verb);
        String[] endings = endingsAEI(verb);

        //Checks if the verb is irregular
        if(iregs.containsKey(verb)) {
            return print(conjugation, "", iregs.get(verb), pn);
        }

        //Checks if a verb or any subsections of it have change matching that of the first person in the Present Tense
        else if (checkForIreg(verb, yoChange) >= 0) {
            int i = checkForIreg(verb, yoChange);
            return printYo(verb.substring(0, i), yoChange.get(verb.substring(i)), endings, pn);
        }

        //Checks if the verb ends with "cer" or "cir" due to certain exceptions
        else if(endsWithCerCir(verb)) {
            String subst = substZC(verb);
            return print(conjugation, subst, endings, pn);
        }

        //Checks if the verb ends with "uir" due to certain exceptions
        else if(verb.substring(verb.length()-3).equals("uir")) {
            return print(conjugation, verb.substring(0, verb.length()-2) + "y", endings, pn);
        }

        //Checks if the verb ends with "uar" due to certain exceptions
        else if(endsWithUar(verb)) {
            return print(conjugation, verb.substring(0, verb.length()-3) + "ú", root(verb), endings, pn);
        }

        //Checks if a verb ends with "iar" due to an exception
        else if(iarAccented.contains(verb)) {
            return print(conjugation, verb.substring(0, verb.length()-3) + "í", root(verb), endings, pn);
        }
        else if (verb.endsWith("guar")) {
            return print(conjugation, verb.substring(0, verb.length()-3), iregs.get("guar"), pn);
        }
        else {

            //Uses the verb's gerund to handle certain stem changes
            String gerund = Progressive.gerund(verb);
            String withSmallChange = gerund.substring(0, gerund.length()-4);

            //Changes the amount removed from the verb bases on it's endings
            //Due to the difference in length between "ando" and "iendo"
            if((gerund.charAt(gerund.length()-4) != 'a') && (gerund.charAt(gerund.length()-5) != 'ñ')) {
                withSmallChange = gerund.substring(0, gerund.length()-5);
            }

            if (verb.equals("poder")) {
                withSmallChange = "pod";
            }

            return print(conjugation, root(carGarZar(stemChange)), root(carGarZar(withSmallChange+ending(verb))), endings, pn);
        }
    }

    //Prints verbs with a change matching the first person of the Present tense and handles various slight changes
    protected String printYo(String beginning, String verb, String[] ends, int i) {
        return(beginning + verb.substring(0, verb.length()-1) + ends[i]);
    }

    //Prints verbs in the Present Subjunctive tense while handling different stem changes in certain point-of-views
    private String print(Conjugation conjugation, String verb, String withSmallChange, String[] ends, int i) {
        if(i == 3 || i == 4) {
            return(conjugation.toBeReflexive[i] + withSmallChange + ends[i]);
        } else {
            return(conjugation.toBeReflexive[i] + verb + ends[i]);
        }
    }

    protected static HashMap<String, String[]> iregs = new HashMap<String, String[]>();

    //Fills a HashMap of irregular verbs in the Present Subjunctive tense
    private void fillMap() {
        iregs.put("ser", new String[]{"sea", "seas", "sea", "seamos", "seáis", "sean"});
        iregs.put("estar" , new String[]{"esté", "estés", "esté", "estemos", "estéis", "estén"});
        iregs.put("ir" , new String[]{"vaya", "vayas", "vaya", "vamos", "vayáis", "vayan"});
        iregs.put("dar" , new String[]{"dé", "des", "dé", "demos", "deis", "den"});
        iregs.put("saber" , new String[]{"sepa", "sepas", "sepa", "sepamos", "sepáis", "sepan"});
        iregs.put("ir" , new String[]{"vaya", "vayas", "vaya", "vayamos", "vayáis", "vayan"});
        iregs.put("haber" , new String[]{"haya", "hayas", "haya", "hayamos", "hayáis", "hayan"});
        iregs.put("reir" , new String[]{"ría", "rías", "ría", "riamos", "riáis", "rían"});
        iregs.put("sonreir" , new String[]{"sonría", "sonrías", "sonría", "sonriamos", "sonriáis", "sonrían"});
        iregs.put("freir" , new String[]{"fría", "frías", "fría", "friamos", "friáis", "frían"});
        iregs.put("errar", new String[]{"yerre","yerres","yerre","erremos","erréis","yerren"});
        iregs.put("prever", new String[]{"prevea","preveas","prevea","preveamos","preveáis","prevean"});
        iregs.put("ver", new String[]{"vea","veas","vea","veamos","veáis","vean"});
        iregs.put("prohibir", new String[]{"prohíba","prohíbas","prohíba","prohibamos","prohibáis","prohíban"});
        iregs.put("rehusar", new String[]{"rehúse","rehúses","rehúse","rehusemos","rehuséis","rehúsen","rehusando"});
        iregs.put("oler", new String[]{"huela","huelas","huela","olamos","oláis","huelan"});
        iregs.put("torcer", new String[]{"tuerza","tuerzas","tuerza","torzamos","torzáis","tuerzan"});
        iregs.put("tropezar", new String[]{"tropiece","tropieces","tropiece","tropecemos","tropezéis","tropiecen","tropezando"});
        iregs.put("guar", new String[]{"üe", "ües", "üe", "üemos", "üéis", "üen"});
    }
}
