module Slabs

export Slab

using PrettyTables
using StructArrays
using Tables

#####
##### Utilities
#####

# Sort NamedTuples alphabetically
ntsort(x::NamedTuple) = (;sort([pairs(x)...]; by = x -> first(x))...)

function ismatch(a::NamedTuple, b::NamedTuple)
    # Can only match if `b` is possibly a subset of `a`.
    issubset(keys(b), keys(a)) || return false
    for (k, v) in pairs(b)
        # The "data" field is special - don't compare here for equality
        k == :data && continue
        a[k] == v || return false
    end
    return true
end

# Add in `nothing` entries
function expand(@nospecialize(nt::NamedTuple), names; f = () -> nothing)
    newnames = Tuple(setdiff(names, keys(nt)))
    nothing_tuple = NamedTuple{newnames}(ntuple(i -> f(), length(newnames)))
    expanded_nt = merge(nt, nothing_tuple)
    return ntsort(expanded_nt)
end

# Type expanding version push!
pushconvert(a::Vector{T}, b::V) where {T, V} = push!(convert(Vector{promote_type(T,V)}, a), b)

#####
##### Slab
#####

"""
Indexing
--------

* `S[i::Integer] -> NamedTuple`: `i`th row of the StructArray
* `S[nt::NamedTuple] -> Slab`: Filter for all entries matching `nt`.
* `S[;kw...] -> Slab`: Syntax shorthand for `S[::NamedTuple]`.
* `S[s::Symbol] -> Vector`: Return column `s`.
* `S[t::NTuple{N,Symbol}] -> Vector{<:Tuple}`: Return columns for each entry in `t`.
"""
mutable struct Slab
    slab::StructArray
end
Slab() = Slab(StructArrays.StructArray())

# Wrap some methods
Base.length(S::Slab) = length(S.slab)
Base.iterate(S::Slab) = iterate(S.slab)
Base.iterate(S::Slab, state) = iterate(S.slab, state)
Base.eltype(S::Slab) = eltype(S.slab)

# ALL of the various getindexing behaviors
Base.getindex(S::Slab, i::Integer) = S.slab[i]
Base.getindex(S::Slab, i) = Slab(StructArray(S.slab[i]))
Base.getindex(S::Slab, ::Nothing) = S
Base.getindex(S::Slab, nt::NamedTuple) = S[findall(x -> ismatch(x, nt), S.slab)]
Base.getindex(S::Slab; kw...) = S[(;kw...)]
Base.getindex(S::Slab, s::Symbol) = getproperty(S.slab, s)
Base.getindex(S::Slab, s::NTuple{N,Symbol}) where {N} = [getproperty.(Ref(x), s) for x in S]

# Hook for customizing merging.
datamerge(a, b) = b

function Base.setindex!(
        slab::Slab,
        dict::Dict{Symbol},
        @nospecialize(nt::NamedTuple),
    )

    structarray = slab.slab
    data = Dict{Symbol,Any}(dict)

    # This field is reserved. Keep myself from being an idiot.
    @assert !haskey(nt, :data)

    # Expand the new entry so it has all the keys in the original database.
    nt = expand(merge(nt, (data = data,)), propertynames(structarray))

    # If the names of `nt` are a subset of the keys already in `db` - we may be able to merge.
    if issubset(keys(nt), propertynames(structarray))
        local savedrow
        found = false
        for row in Tables.rows(structarray)
            if ismatch(row, nt)
                found == true && error("Found Multiple Matches for $nt")

                savedrow = row
                found = true
            end
        end

        if found
            # Merge the data entries
            merge!(datamerge, savedrow.data, nt.data)
            return slab
        end
    end

    # If the slab is empty, just wrap up the current entry
    if iszero(length(structarray))
        slab.slab = StructArray([nt])
        return slab
    end

    # Otherwise, we have to do a bunch of promotion on everything.
    arrays = expand(
        StructArrays.fieldarrays(structarray),
        keys(nt);
        f = () -> fill(nothing, length(structarray))
    )

    # Make sure the directions here are still equal.
    @assert keys(arrays) == keys(nt)
    conversions = pushconvert.(Tuple(arrays), Tuple(nt))

    # Update the wrapped array and return.
    arrays = NamedTuple{keys(arrays)}(conversions)
    slab.slab = StructArray(arrays)
    return slab
end

function Base.show(io::IO, slab::Slab)
    if iszero(length(slab))
        print(io, "Empty Slab")
        return nothing
    end

    sch = Tables.schema(slab.slab)
    header = filter(!isequal(:data), collect(sch.names))

    vectors = (getproperty(slab.slab, i) for i in header)
    pretty_table(
        io,
        hcat(vectors...),
        header;
        # Let this thing get tall.
        crop = :horizontal,
    )
end

end # module
