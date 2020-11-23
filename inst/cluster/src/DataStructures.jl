# Copied directly from DataStructures.jl, but specialized
# to always make the parent be the smallest label
struct DisjointMinSets
    parents::Vector{Int}

    DisjointMinSets(n::Integer) = new([1:n;])
end
DisjointMinSets() = DisjointMinSets(0)

function find_root!(sets::DisjointMinSets, m::Integer)
    p = sets.parents[m]   # don't use @inbounds here, it might not be safe
    @inbounds if sets.parents[p] != p
        sets.parents[m] = p = find_root_unsafe!(sets, p)
    end
    p
end

# an unsafe variant of the above
function find_root_unsafe!(sets::DisjointMinSets, m::Int)
    @inbounds p = sets.parents[m]
    @inbounds if sets.parents[p] != p
        sets.parents[m] = p = find_root_unsafe!(sets, p)
    end
    p
end

function union!(sets::DisjointMinSets, m::Integer, n::Integer)
    mp = find_root!(sets, m)
    np = find_root!(sets, n)
    if mp < np
        sets.parents[np] = mp
        return mp
    elseif np < mp
        sets.parents[mp] = np
        return np
    end
    mp
end

function push!(sets::DisjointMinSets)
    m = length(sets.parents) + 1
    push!(sets.parents, m)
    m
end
