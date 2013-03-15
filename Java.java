import java.util.Random;
import java.util.ArrayList;
class Java {

    public static void main(String[] args) {

        
    }
}



class Application {

    public Application() {

    }

    public void run(String goal, int mutationRate, int numberOfChildren) {

    }

}

// import java.lang.StringBuilder; 
class Individual {

    private String lookslike;

    public Individual(String lookslike) {
        this.lookslike = lookslike;
    }

    public ArrayList<Individual> reproduceChildren(int mutationRate, int numberOfChildren) {
        return new ArrayList<Individual>();
    }


    public String reproduceChild(int mutationRate) {
        StringBuilder builder = new StringBuilder();
        for(char character : lookslike.toCharArray()) {
            builder.append(mutateCharacter(character, mutationRate));
            
        }
        return builder.toString();
    }

    private char mutateCharacter(char character, int mutationRate) {
        Random random = new Random();

        if(mutationRate == random.nextInt(mutationRate)+1) {
            return (char)(random.nextInt(26) + 'A');
        } else {
            return character;
        }

        

    }

}


