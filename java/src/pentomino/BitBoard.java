package pentomino;

public class BitBoard {
    private BitBoard() {
    }

    public static long deltaSwap(long x, long mask, int delta) {
        long t = ((x >> delta) ^ x) & mask;
        return t ^ (t << delta) ^ x;
    }
}
