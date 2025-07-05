
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Arrays;

class Square {
    int r, c;

    public Square(int a, int b)
    {
        r = a; c = b;
    }
}


public class Queens extends Game {
    ArrayList<Square> squares;

    public Queens(int n)
    {
        squares = new ArrayList<Square>();

        for (int r = 0; r < n; r++){
            for (int c = 0; c < n; c++){
                squares.add(new Square(r, c));
            }
        }
    }

    public static boolean areSquaresAligned(Square a, Square b)
    {
        int rd = a.r - b.r, cd = a.c - b.c;
        return rd == 0 || cd == 0 || Math.abs(rd) == Math.abs(cd);
    }

    public Queens afterMove(Square sq)
    {
        ArrayList<Square> newSquares = new ArrayList<Square>();
        for (Square sq2 : squares){
            if (!areSquaresAligned(sq, sq2))
                newSquares.add(sq2);
        }

        Queens option = new Queens(0);
        option.squares = newSquares;
        return option;
    }

    public Game[] options()
    {
        ArrayList<Queens> options = new ArrayList<Queens>();
        for (Square sq : squares){
            options.add(afterMove(sq));
        }

        Queens[] optionArray = options.toArray(new Queens[0]);
        Arrays.sort(optionArray, new Comparator<Queens>(){
            public int compare(Queens a, Queens b){
                return a.squares.size() - b.squares.size();
            }
        });

        return optionArray;
    }



    public static void main(String[] args){
        int n = Integer.parseInt(args[0]);
        System.out.println(new Queens(n).calculateNimber());
    }
}

