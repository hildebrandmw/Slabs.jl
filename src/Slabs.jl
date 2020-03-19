module Slabs

using DataStructures
using Tables
using PrettyTables

# A row in the table
struct Entry <: Tables.AbstractRow
    parameters::SortedDict{Symbol,Any}
    data::Dict{String,Any}
end
parameters(x::Entry) = getfield(x, :parameters)
Base.:(==)(x::Entry, y::Entry) = parameters(x) == parameters(y)

struct Sentinel end
function Base.:(==)(x::SortedDict{Symbol,Any}, y::Entry)
    params = parameters(y)
    for (k,v) in x
        val = get(params, k, Sentinel())
        val == v || return false
    end
    return true
end
Base.keys(x::Entry) = keys(parameters(x))

Base.getindex(x::Entry) = getfield(x, :data)
Base.getindex(x::Entry, nm::Symbol) = parameters(x)[nm]

# Table API
Tables.getcolumn(x::Entry, nm::Symbol) = x[nm]
Tables.getcolumn(x::Entry, i::Integer) = Tables.getcolumn(x, collect(keys(x))[i])
Tables.columnnames(x::Entry) = collect(keys(x))

# Adding empty fields to an Entry
function expand!(d::SortedDict{Symbol,Any}, ks)
    for k in ks
        get!(d, k, nothing)
    end
    return nothing
end
expand!(E::Entry, ks) = expand!(parameters(E), ks)

#####
##### Slab
#####
struct Slab
    entries::Vector{Entry}
    keys::Vector{Symbol}
end
Base.keys(slab::Slab) = slab.keys

Tables.istable(::Type{Slab}) = true
Tables.rowaccess(::Type{Slab}) = true
Tables.rows(slab::Slab) = slab.entries

Slab() = Slab(Entry[], Symbol[])

datamerge(a, b) = b
function Base.setindex!(slab::Slab, data::Dict{String}, parameters::Dict{Symbol})
    parameters = SortedDict{Symbol,Any}(parameters)
    data = Dict{String,Any}(data)

    # Add in all of the keys from the slab.
    expand!(parameters, keys(slab))
    entry = Entry(parameters, data)

    # If the keys of this new entry are a subset of the keys already existing in the slab,
    # then we have to check if this entry already exists in the Slab.
    if issubset(keys(parameters), keys(slab))
        i = findfirst(isequal(entry), Tables.rows(slab))
        if !isnothing(i)
            merge!(datamerge, slab[i].data, data)
            return nothing
        end
    else
        # Expand the existing entries for any new keys that were potentially created.
        expand!.(Tables.rows(slab), Ref(keys(entry)))
        append!(keys(slab), setdiff(keys(entry), keys(slab)))
        sort!(keys(slab))
    end

    # Now we add a new row.
    push!(slab.entries, entry)
    return nothing
end

# Indexing methods for searching and reducing the size of the table.
Base.length(x::Slab) = length(x.entries)
Base.iterate(x::Slab) = iterate(x.entries)
Base.iterate(x::Slab, s) = iterate(x.entries, s)
Base.eltype(x::Slab) = eltyps(x.entries)

# `getindex` Overloading
_todict(x::NamedTuple) = SortedDict{Symbol,Any}(k => v for (k,v) in pairs(x))
Base.getindex(x::Slab, i::Integer) = x.entries[i]
Base.getindex(x::Slab, i) = Slab(x.entries[i], x.keys)
Base.getindex(x::Slab, ::Nothing) = Slab()
function Base.getindex(x::Slab, i::SortedDict{Symbol,Any})
    return x[findall(x -> isequal(i, x), x.entries)]
end
Base.getindex(x::Slab, i::NamedTuple) = x[_todict(i)]
Base.getindex(x::Slab; kw...) = x[(;kw...,)]
Base.getindex(x::Slab, s::Symbol) = [ e[s] for e in x ]

Base.in(d::Dict, s::Slab) = any(x -> isequal(SortedDict{Symbol,Any}(d), x), s.entries)

#####
##### Tables Interface
#####

# Pretty Printing
function Base.show(io::IO, slab::Slab)
    if length(slab) == 0
        println("Empty Slab")
    else
        PrettyTables.pretty_table(io, slab, keys(slab))
    end
end

end # module
