# dataset from https://snap.stanford.edu/data/com-Amazon.html
set.seed(42)
library(igraph)
library(Matrix)

relations = read.table('data/com-amazon.ungraph.txt')
community.top5000.path = 'data/com-amazon.top5000.cmty.txt'
temp = max(count.fields(community.top5000.path))
community.top5000 = read.table(community.top5000.path, fill = T, col.names = 1:temp)
community.top5000.sizes = rowSums(!is.na(community.top5000))

flattened = na.omit(unlist(community.top5000))
flattened.table = table(flattened)

# vertices in top 5000
vertices = names(flattened.table)
vertices.count = length(vertices)
vertices.is.single = flattened.table == 1 # unused experiment

# edges in top 5000
relations = relations[(relations$V1 %in% vertices) & (relations$V2 %in% vertices),]

# 80:20 split
vertices.count.seq = seq_len(vertices.count)
train.index = sample(vertices.count.seq, size = round(vertices.count*.8))
vertices.is.train = vertices.count.seq %in% train.index

# as communities are overlapping, label vertices with the largest community it is in
filter = apply(community.top5000, 1, function (c) vertices %in% c) # are vertices in each community?
labels = apply(filter, 1, function (f) { # filter of communities that contains the vertex
        max.which = which.max(community.top5000.sizes[f])[1]
        which(f)[max.which] # index of largsest community that contains
})
vertices.init = rep(-1, length(vertices))
vertices.init[vertices.is.train] = labels[vertices.is.train] # for training
actual = labels[!vertices.is.train] # for prediction

g = graph.data.frame(relations, directed = F) # make graph
g = permute(g, invPerm(order(as.numeric(V(g)$name)))) # sort vertices to match external indexing

# perform label propagation
# fixed was experimented with fixed = vertices.is.train & vertices.is.single (accuracy:  0.9970087)
# to prevent multi labelled vertices (overlaps) from being fixed
# however, accuracy is sightly better by fixing all training vertices
community.model = label.propagation.community(g, initial = vertices.init, fixed = vertices.is.train)
community.count = length(community.model)
community.ms = membership(community.model)
community.members = sapply(1:community.count, function (i) names(community.ms[community.ms == i]))
community.map = sapply(community.members, function (l) {
        c = sapply(l, function (v) {
                vertices.init[which(vertices == v)]
        })
        t = table(c[c > 0])
        names(t)[which.max(t)]
})

pred = unname(sapply(vertices[!vertices.is.train], function (v) {
        # v must be character
        community.map[community.ms[v]]
}))

correct = sum(pred == actual)
total = length(pred)

cat('correct: ', correct, '\n')
cat('total: ', total, '\n')
cat('accuracy: ', correct / length(pred), '\n')