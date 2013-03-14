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


  private String reproduceChild(String parent, int mutationRate) {
    // StringBuilder builder = new StringBuilder();
    for(char character : parent.toCharArray()) {
      //CharacterIterator ist?
      
    }
    return "";
  }

}


