    import java.util.Random;
class Java {

    public static void main(String[] args) {

        
    }
}



class Application {
    private String goal;
    private int mutationRate;
    private int numberOfChildren;

    public Application(String goal, int mutationRate, int numberOfChildren) {
        this.goal = goal;
        this.mutationRate = mutationRate;
        this.numberOfChildren = numberOfChildren;
    }

}

// import java.lang.StringBuilder; 
class Parent {

    private String lookslike;

    public Parent(String lookslike) {
        this.lookslike = lookslike;
    }

    public String reproduceFavouriteChild(String goal, int mutationRate, int numberOfChildren) {
        return "";
    }


    public String reproduceChild(String parent, int mutationRate) {
        StringBuilder builder = new StringBuilder();
        for(char character : parent.toCharArray()) {
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


