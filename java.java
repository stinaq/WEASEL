import java.util.Random;
import java.util.ArrayList;

class Application {
    public static void main(String[] args) {
        WeaselWorld weaselWorld = new WeaselWorld("ABCDEFGHIJKLMNOPQRSTUVWXYZ ", "METHINKS IT IS LIKE A WEASEL");
        weaselWorld.createWeasel(0.04d, 100);
    }
}

class WeaselWorld {
    private final String alphabet;
    private final String goal;


    public WeaselWorld (String alphabet, String goal) {
        this.alphabet = alphabet;
        this.goal = goal;
    }

    public void createWeasel (double mutationRate, int numberOfChildren) {
        Individual sire = createSire();
        System.out.println(sire.getLookslike());
        
        int generations = 1;


        while (!sire.getLookslike().equals(goal)) {
            ArrayList<Individual> children = sire.reproduceChildren(mutationRate, numberOfChildren, alphabet);
            int bestAlikeness = 0;
            
            for (Individual child : children) {
                int alikeness = calculateAlikeness(child.getLookslike());
                if (alikeness > bestAlikeness) {
                    bestAlikeness = alikeness;
                    sire = child;
                }
            }
            generations++;
            System.out.println(sire.getLookslike());
        }
        System.out.println("Finished at generation " + generations);
    }

    private Individual createSire () {
        StringBuilder builder = new StringBuilder();
        Random random = new Random();
        for(int i = 0; i < goal.length(); i++) {
            char randomChar = alphabet.charAt(random.nextInt(alphabet.length()));
            builder.append(randomChar);    
        }
        Individual sire = new Individual(builder.toString());
        return sire;
    } 

    private int calculateAlikeness (String child) {
        int alikeness = 0;
        for (int i = 0; i < child.length(); i++) {
            if (child.charAt(i) == goal.charAt(i)) alikeness++;
        }
        return alikeness;
    }
}

class Individual {

    private String lookslike;
    private Random random = new Random();

    public String getLookslike () {
        return this.lookslike;
    }

    public Individual (String lookslike) {
        this.lookslike = lookslike;
    }

    public ArrayList<Individual> reproduceChildren (double mutationRate, int numberOfChildren, String alphabet) {
        ArrayList<Individual> children = new ArrayList<Individual>();
        for (int i = 0; i < numberOfChildren; i++) {
            Individual child = reproduceChild(mutationRate, alphabet);
            children.add(child);
        }
        return children;
    }

    public Individual reproduceChild (double mutationRate, String alphabet) {
        StringBuilder builder = new StringBuilder();
        for(char character : lookslike.toCharArray()) {
            builder.append(mutateCharacter(character, mutationRate, alphabet));
        }
        return new Individual(builder.toString());
    }

    private char mutateCharacter (char character, double mutationRate, String alphabet) {
        if(random.nextDouble() < mutationRate) {
            return alphabet.charAt(random.nextInt(alphabet.length()));
        } else {
            return character;
        }
    }
}


