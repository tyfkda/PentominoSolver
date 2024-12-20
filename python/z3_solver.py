import z3

def exactlyOne(solver, vars):
    solver.add(z3.Or(vars))
    for i in range(len(vars)):
        for j in range(i + 1, len(vars)):
            solver.add(z3.Or(z3.Not(vars[i]), z3.Not(vars[j])))

class Z3Solver:
    def __init__(self, board_w, board_h, pieces):
        self.board_w = board_w
        self.board_h = board_h
        self.pieces = pieces

    def solve(self):
        prb = []
        for piece in self.pieces:
            bb = []
            for shape in piece.shapes:
                w = len(shape[0])
                h = len(shape)
                for y in range(self.board_h - h + 1):
                    for x in range(self.board_w - w + 1):
                        b = []
                        for i in range(h):
                            for j in range(w):
                                if shape[i][j] == '@':
                                    b.append((y + i) * self.board_w + (x + j))
                        bb.append(b)
            prb.append(bb)

        xs = []
        for i in range(len(prb)):
            x = [z3.Bool('x_{}_{}'.format(i, j))
                 for j in range(len(prb[i]))]
            xs.append(x)

        # print(xs)

        solver = z3.Solver()

        # 制約条件１
        #１つのピースの中では、置き方を１つだけ選択できる
        for i in range(len(xs)):
            print(f'### Constraint Piece:{i}')
            exactlyOne(solver, xs[i])

        # 制約条件２
        #盤面に重ならないように配置する
        for i in range(0, self.board_w * self.board_h):
            bs = []
            for j, itemj in enumerate(prb):
                for k, itemk in enumerate(itemj):
                    if i in itemk:
                        bs.append(xs[j][k])
            print(f'### Constraint Position:{i}')
            exactlyOne(solver, bs)

        # print(solver)

        # 解を取得
        while True:
            print('### Check')
            if solver.check() != z3.sat:
                break

            sol = solver.model()
            ans = []
            for i, xs1 in enumerate(xs):
                ans_bin = [z3.is_true(sol[x]) for x in xs1]
                j = ans_bin.index(True)
                ans.append((j, prb[i][j]))
            yield ans

            # 今回見つけた解を無効にする
            solver.add(z3.Or([z3.Not(xs[i][j]) for i, (j, _) in enumerate(ans)]))
