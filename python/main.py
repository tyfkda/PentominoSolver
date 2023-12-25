from piece import create_pieces
import pulp_solver

def main():
    # (board_w, board_h) = (10, 6)
    (board_w, board_h) = (20, 3)
    pieces = create_pieces(board_w, board_h)
    solver = pulp_solver.PulpSolver(board_w, board_h, pieces)

    total = 0
    for result in solver.solve():
        s = ['.'] * (board_w * board_h)
        for i, (_j, poss) in enumerate(result):
            for b in poss:
                s[b] = pieces[i].name

        total += 1
        print(f'### {total}')
        for y in range(board_h):
            print(''.join(s[y * board_w: (y + 1) * board_w]))
        print()

    print(f'total=#{total}')

if __name__ == '__main__':
    main()
