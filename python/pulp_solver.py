import pulp

class PulpSolver:
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
                # if w > board_w or h > board_h:
                #     continue
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
            x = [pulp.LpVariable('x_{}_{}'.format(i, j), cat='Binary')
                 for j in range(len(prb[i]))]
            xs.append(x)

        prob = pulp.LpProblem("pentomino", pulp.LpMinimize)

        # 制約条件１
        #１つのピースの中では、置き方を１つだけ選択できる
        for i in range(len(xs)):
            prob += pulp.lpSum(xs[i]) == 1

        # 制約条件２
        #盤面に重ならないように配置する
        for i in range(0, self.board_w * self.board_h):
            bs = []
            for j, itemj in enumerate(prb):
                for k, itemk in enumerate(itemj):
                    if i in itemk:
                        bs.append(xs[j][k])
            prob += pulp.lpSum(bs) == 1

        while True:
            #PULPで解く
            solver = pulp.PULP_CBC_CMD(msg=0, threads=8)
            result = prob.solve(solver)
            if result != 1:  # Not found
                break

            # 解を取得
            ans = []
            for i, xs1 in enumerate(xs):
                ans_bin = [x.value() for x in xs1]
                j = ans_bin.index(1.0)
                ans.append((j, prb[i][j]))
            yield ans

            # 今回見つけた解を無効にする
            bs = [xs[i][ps] for i, (ps, _) in enumerate(ans)]
            prob += pulp.lpSum(bs) != 12  #len(bs)
