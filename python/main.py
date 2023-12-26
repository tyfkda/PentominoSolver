import argparse

from piece import create_pieces
import pulp_solver
import z3_solver

def main():
    parser = argparse.ArgumentParser(prog='Pentomino Solver')
    parser.add_argument('--solver', choices=['pulp', 'z3'], default='pulp')
    args = parser.parse_args()
    # print(args)
    # # return


    # (board_w, board_h) = (10, 6)
    (board_w, board_h) = (20, 3)
    pieces = create_pieces(board_w, board_h)
    if args.solver == 'z3':
        solver = z3_solver.Z3Solver(board_w, board_h, pieces)
    else:
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
