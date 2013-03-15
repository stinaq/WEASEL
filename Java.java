import java.util.Random;
import java.util.ArrayList;
class Java {

    public static void main(String[] args) {
        Application a = new Application();
        a.run("ipgfuh", 4, 10);
        
    }
}



class Application {

    public void run (String goal, int mutationRate, int numberOfChildren) {
        Individual i = new Individual("hieurh");
        ArrayList<Individual> children = i.reproduceChildren(mutationRate, numberOfChildren);
        for (Individual c : children) {
            System.out.println(c.getLookslike());
            
        }
    }

}

// import java.lang.StringBuilder; 
class Individual {

    private String lookslike;

    public String getLookslike () {
        return this.lookslike;
    }

    public Individual (String lookslike) {
        this.lookslike = lookslike;
    }

    public ArrayList<Individual> reproduceChildren(int mutationRate, int numberOfChildren) {
        ArrayList<Individual> children = new ArrayList<Individual>();
        for (int i = 0; i <= numberOfChildren; i++) {
            Individual child = new Individual(reproduceChild(mutationRate));
            children.add(child);
        }


        return children;
    }


    private String reproduceChild (int mutationRate) {
        StringBuilder builder = new StringBuilder();
        for(char character : lookslike.toCharArray()) {
            builder.append(mutateCharacter(character, mutationRate));
            
        }
        return builder.toString();
    }

    private char mutateCharacter (char character, int mutationRate) {
        Random random = new Random();

        if(mutationRate == random.nextInt(mutationRate)+1) {
            return (char)(random.nextInt(26) + 'A');
        } else {
            return character;
        }

        

    }

}


