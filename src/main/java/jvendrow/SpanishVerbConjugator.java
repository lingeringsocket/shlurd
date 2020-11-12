package jvendrow;

public class SpanishVerbConjugator extends Endings
{
    public static Present present = new Present(presentAr, presentEr, presentIr);
    public static Preterite preterite = new Preterite(preteriteAr, preteriteEr, preteriteIr);
    public static Imperfect imperfect = new Imperfect(imperfectAr, imperfectErIr, imperfectErIr);
    public static Future futureSimple = new Future(futureErIr, futureErIr, futureErIr);
    public static Conditional conditionalSimple = new Conditional(conditional);
    public static Perfect presentPerfect = new Perfect("Present");
    public static Perfect imperfectPerfect = new Perfect("Imperfect");
    public static Perfect futurePerfect = new Perfect("Future");
    public static Perfect conditionalPerfect = new Perfect("Conditional");
    public static Perfect subjunctivePerfect = new Perfect("Subjunctive");
    public static Perfect imperfectSubjunctivePerfect = new Perfect("ImperfectSubjunctive");
    public static Progressive presentProgressive = new Progressive("Present");
    public static Progressive imperfectProgressive = new Progressive("Imperfect");
    public static Progressive preteriteProgressive = new Progressive("Preterite");
    public static Progressive futureProgressive = new Progressive("Future");
    public static PresentSubjunctive presentSubjunctive = new PresentSubjunctive(subjunctiveAr, subjunctiveErIr, subjunctiveErIr);
    public static ImperfectSubjunctive imperfectSubjunctive = new ImperfectSubjunctive(imperfectSubjunctiveAr, imperfectSubjunctiveErIr, imperfectSubjunctiveErIr);
    public static Commands commandsPositive = new Commands(subjunctiveAr, subjunctiveErIr, subjunctiveErIr, "Positive");
    public static Commands commandsNegative = new Commands(subjunctiveAr, subjunctiveErIr, subjunctiveErIr, "Negative");

    public static String conjugate(
        String infinitive, Tense tense, int person, boolean plural)
    {
        Tense.Conjugation conjugation = new Tense.Conjugation();
        conjugation.pn = person;
        if (plural) {
            conjugation.pn += 3;
        }
        conjugation.verb = infinitive;
        if(infinitive.substring(infinitive.length()-2).equals("se")) {
            conjugation.isReflexive = true;
            conjugation.toBeReflexive = Tense.reflexive;
            conjugation.verb = Tense.root(infinitive);
        }
        else {
            conjugation.isReflexive = false;
            conjugation.toBeReflexive = Tense.toBeReflexive;
        }
        conjugation.original = conjugation.verb;
        if(Tense.end(conjugation.verb) == 'í') {
            conjugation.verb = Tense.root(conjugation.verb) + "ir";
        }
        return Tense.fixOrthography(tense.conjugate(conjugation));
    }
}
