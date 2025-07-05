
import java.util.ArrayList;

public class JoinedGame extends Game {
    public Game g;
    public int nimheap;

    public JoinedGame(Game g, int nimheap){
        this.g = g;
        this.nimheap = nimheap;
    }



    public Game[] options()
    {
        ArrayList<Game> a = new ArrayList<Game>();

        for (Game h : g.options()){
            a.add(new JoinedGame(h, nimheap));
        }

        for (int h = 0; h < nimheap; h++){
            a.add(new JoinedGame(g, h));
        }

        return (Game[]) a.toArray(new Game[0]);
    }
}

