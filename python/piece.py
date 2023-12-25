class Piece:
    def __init__(self, name, shapes):
        self.name = name
        self.shapes = shapes

    def __str__(self):
        return f"Piece({self.name}, {self.shapes})"

original_shapes = [
    {
        'name': 'F',
        'lines': [
            " @@",
            "@@ ",
            " @ ",
        ],
    },
    {
        'name': 'I',
        'lines': [
            "@",
            "@",
            "@",
            "@",
            "@",
        ],
    },
    {
        'name': 'L',
        'lines': [
            "@ ",
            "@ ",
            "@ ",
            "@@",
        ],
    },
    {
        'name': 'N',
        'lines': [
            " @",
            " @",
            "@@",
            "@ ",
        ],
    },
    {
        'name': 'P',
        'lines': [
            "@@",
            "@@",
            "@ ",
        ],
    },
    {
        'name': 'T',
        'lines': [
            "@@@",
            " @ ",
            " @ ",
        ],
    },
    {
        'name': 'U',
        'lines': [
            "@ @",
            "@@@",
        ],
    },
    {
        'name': 'V',
        'lines': [
            "@  ",
            "@  ",
            "@@@",
        ],
    },
    {
        'name': 'W',
        'lines': [
            "@  ",
            "@@ ",
            " @@",
        ],
    },
    {
        'name': 'X',
        'lines': [
            " @ ",
            "@@@",
            " @ ",
        ],
    },
    {
        'name': 'Y',
        'lines': [
            " @",
            "@@",
            " @",
            " @",
        ],
    },
    {
        'name': 'Z',
        'lines': [
            "@@ ",
            " @ ",
            " @@",
        ],
    },
]

def create_shape_from_lines(lines):
    return [list(line) for line in lines]

def rot90(shape):
    result = []
    w = len(shape[0])
    h = len(shape)
    for y in range(0, w):
        line = []
        for x in reversed(range(0, h)):
            line.append(shape[x][y])
        result.append(line)
    return result

def flip_y(shape):
    shape.reverse()

def rot_flip(lines, flip, rot):
    shape = create_shape_from_lines(lines)
    if flip:
        flip_y(shape)
    for _ in range(rot):
        shape = rot90(shape)
    return shape

def create_shape(name, lines, board_w, board_h):
    shapes = []
    for i in range(0, 8):
        rotflipped = rot_flip(lines, i >= 4, i & 3)
        if len(rotflipped[0]) > board_w or len(rotflipped) > board_h:
            continue
        if all([s != rotflipped for s in shapes]):
            shapes.append(rotflipped)
    return Piece(name, shapes)

def create_pieces(board_w, board_h):
    return [create_shape(pat['name'], pat['lines'], board_w, board_h) for pat in original_shapes]
