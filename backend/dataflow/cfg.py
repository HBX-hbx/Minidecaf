from backend.dataflow.basicblock import BasicBlock

"""
CFG: Control Flow Graph

nodes: sequence of basicblock
edges: sequence of edge(u,v), which represents after block u is executed, block v may be executed
links: links[u][0] represent the Prev of u, links[u][1] represent the Succ of u,
"""


class CFG:
    def __init__(self, nodes: list[BasicBlock], edges: list[(int, int)]) -> None:
        self.nodes = nodes
        self.edges = edges
        self.reachableNodes = [False] * len(self.nodes) # flag for all nodes, indicates whether can be reached
        self.links = []

        for i in range(len(nodes)):
            self.links.append((set(), set()))

        for (u, v) in edges:
            self.links[u][1].add(v)
            self.links[v][0].add(u)

        # bfs
        import queue
        queue = queue.Queue()
        queue.put(0) # add the first node
        while not queue.empty():
            curNodeId = queue.get() # get the front node
            self.reachableNodes[curNodeId] = True
            for id in self.getSucc(curNodeId): # trace all the succ nodes
                if not self.reachableNodes[id]: # if not reachable
                    queue.put(id) # add into the queue

    def getBlock(self, id):
        return self.nodes[id]

    def getPrev(self, id):
        return self.links[id][0]

    def getSucc(self, id):
        return self.links[id][1]

    def getInDegree(self, id):
        return len(self.links[id][0])

    def getOutDegree(self, id):
        return len(self.links[id][1])

    def iterator(self):
        return iter(self.nodes)

    def reachable(self, id): # tell whether the block self.nodes[id] can be reached
        return self.reachableNodes[id]
