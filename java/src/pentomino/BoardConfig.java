package pentomino;

public enum BoardConfig {
    _6x10(6, 10, 0),
    _5x12(5, 12, 0),
    _4x15(4, 15, 0),
    _3x20(3, 20, 0),
    _8x8(8, 8, (1L << (3 * 8 + 3)) | (1L << (3 * 8 + 4)) | (1L << (4 * 8 + 3)) | (1L << (4 * 8 + 4)));

    public final int width;
    public final int height;
    public final long initialBoard;

    BoardConfig(int width, int height, long initialBoard) {
        this.width = width;
        this.height = height;
        this.initialBoard = initialBoard;
    }
}
