package jvendrow;

public class SpanishVerbConjugator extends Endings
{
    public static Present present = new Present(presentAr, presentEr, presentIr);
    public static Preterite preterite = new Preterite(preteriteAr, preteriteEr, preteriteIr);
    public static Imperfect imperfect = new Imperfect(imperfectAr, imperfectErIr, imperfectErIr);
    public static Future futureSimple = new Future(futureErIr, futureErIr, futureErIr);
    public static Conditional conditionalSimple = new Conditional(conditional);
    public static PresentSubjunctive presentSubjunctive = new PresentSubjunctive(subjunctiveAr, subjunctiveErIr, subjunctiveErIr);
    public static ImperfectSubjunctive imperfectSubjunctive = new ImperfectSubjunctive(imperfectSubjunctiveAr, imperfectSubjunctiveErIr, imperfectSubjunctiveErIr);
    public static Commands commands = new Commands(subjunctiveAr, subjunctiveErIr, subjunctiveErIr);

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
