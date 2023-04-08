package domain.entities.ssim;

import java.io.File;

public class Main {
    public static void main(String[] args) throws Exception {
        SsimCalculator ssim = new SsimCalculator(new File("src/main/scala/resources/ssim/charmander.png"));
        
        System.out.println("Similitud entre fusión y pokemon starter: " + ssim.compareTo(new File("src/main/scala/resources/ssim/bulbasaur.png")));
    }
}
