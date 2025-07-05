
public abstract class Game {
    public abstract Game[] options();

    public Game[] split()
    {
        return null;
    }


    public boolean isGameAWin()
    {
        Game[] options = options();
        for (Game option : options){
            if (option.isGameALoss())
                return true;
        }
        return false;
    }

    public boolean isGameALoss()
    {
        return !isGameAWin();
    }



    public int calculateNimber()
    {
        int candidate = 0;
        while (true){
            if (doesEqualNimber(candidate)){
                return candidate;
            }
            candidate++;
        }
    }

    public boolean doesEqualNimber(int n)
    {
        return new JoinedGame(this, n).isGameALoss();
    }
}

