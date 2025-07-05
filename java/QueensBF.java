
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Arrays;

public class QueensBF extends Game {
    BigInteger freeSquares;
    int n;
    BigInteger masks[];

    public QueensBF(int n)
    {
        freeSquares = new BigInteger("0");
        this.n = n;
        masks = new BigInteger[n*n];

        for (int r = 0; r < n; r++){
            for (int c = 0; c < n; c++){
                freeSquares = freeSquares.setBit(r * n + c);
                masks[r * n + c] = makeMask(r * n + c);
            }
        }
    }

    public BigInteger makeMask(int sq1)
    {
        BigInteger mask = new BigInteger("0");
        for (int sq2 = 0; sq2 < n*n; sq2++){
            if (areSquaresAligned(sq1, sq2)){
                mask = mask.setBit(sq2);
            }
        }
        return mask;
    }

    public boolean areSquaresAligned(int a, int b)
    {
        int rd = a / n - b / n, cd = a % n - b % n;
        return rd == 0 || cd == 0 || Math.abs(rd) == Math.abs(cd);
    }

    public QueensBF afterMove(int sq)
    {
        BigInteger newField = freeSquares.andNot(masks[sq]);

        QueensBF option = new QueensBF(0);
        option.freeSquares = newField;
        option.n = this.n;
        option.masks = this.masks;
        return option;
    }

    public Game[] options()
    {
        ArrayList<QueensBF> options = new ArrayList<QueensBF>();
        for (int sq = 0; sq < n*n; sq++){
            if (freeSquares.testBit(sq)){
                options.add(afterMove(sq));
            }
        }

        QueensBF[] optionArray = options.toArray(new QueensBF[0]);
        Arrays.sort(optionArray, new Comparator<QueensBF>(){
            public int compare(QueensBF a, QueensBF b){
                return a.freeSquares.bitCount() - b.freeSquares.bitCount();
            }
        });

        return optionArray;
    }



    public static void main(String[] args){
        int n = Integer.parseInt(args[0]);
        System.out.println(new QueensBF(n).calculateNimber());
    }
}

