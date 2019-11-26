using Slabs
using Test

# For testing `datamerge`
struct Merger
    val::Int
end
Slabs.datamerge(a::Merger, b::Merger) = Merger(a.val + b.val)

@testset "NamedTuple Utilities" begin
    # NT Sorting
    nt = (b = 1, c = 2, a = 3)
    @test Slabs.ntsort(nt) == (a = 3, b = 1, c = 2)

    #####
    ##### Matching
    #####

    # Expect a match
    a = (a = 1, b = nothing)
    @test Slabs.ismatch(a, (a = 1,))
    @test Slabs.ismatch(a, (a = 1, b = nothing))
    @test Slabs.ismatch(a, (b = nothing, a = 1))
    @test Slabs.ismatch(a, (b = nothing,))

    # Don't expect a match
    @test !Slabs.ismatch(a, (c = 1,))
    @test !Slabs.ismatch(a, (a = 1, b = 1))

    # Make sure the `data` field is not checked.
    a = (a = 1, b = nothing, data = -1234)
    @test Slabs.ismatch(a, (a = 1, data = "hello wold"))

    # Empty NamedTuple should always match
    @test Slabs.ismatch(a, NamedTuple())

    #####
    ##### Expanding
    #####

    # Make sure the result is sorted
    a = (c = 1, b = 2)
    b = Slabs.expand(a, (:a, :d))
    @test b == (a = nothing, b = 2, c = 1, d = nothing)

    # Try again with a different callback.
    b = Slabs.expand(a, (:a, :d); f = () -> 0)
    @test b == (a = 0, b = 2, c = 1, d = 0)

    #####
    ##### pushconvert
    #####

    x = [1,2,3]
    @test Slabs.pushconvert(x, nothing) == [1, 2, 3, nothing]
    @test Slabs.pushconvert(x, "hi") == [1, 2, 3, "hi"]

    #####
    ##### Slab
    #####

    S = Slab()
    println(S)
    @test length(S) == 0
    @test eltype(S) == typeof(NamedTuple())

    data1 = Dict(:merger => Merger(10), :meta => 1)
    S[(a = 1, b = 2)] = data1

    @test length(S) == 1

    # Test some of the indexing methods
    @test S[1] == (a = 1, b = 2, data = data1)
    @test S[nothing] == S
    @test S[:a] == [1]
    @test S[(:a, :b)] == [(1, 2)]

    # Add another entry
    data2 = Dict(:merger => Merger(100), :meta => 2)
    S[(c = 10,)] = data2
    @test propertynames(S.slab) == (:a, :b, :c, :data)
    @test S[1] == (a = 1, b = 2, c = nothing, data = data1)

    # Try filtering
    filt = S[(c = nothing,)]
    @test isa(filt, Slab)
    @test length(filt) == 1
    @test filt[1] == S[1]

    # Do the same thing with the keyword path.
    filt = S[c = nothing]
    @test isa(filt, Slab)
    @test length(filt) == 1
    @test filt[1] == S[1]

    # Some more getindex testing
    @test S[:a] == [1, nothing]
    @test S[(:a, :b)] == [(1, 2), (nothing, nothing)]
    println(S)

    for i in S
        @test isa(i, NamedTuple)
    end

    # Finally, try datamerging.
    data3 = Dict(
        :merger => Merger(100),
        :meta => 2,
        :misc => 0,
    )

    S[(a = 1, b = 2)] = data3
    row = S[(a = 1, b = 2)][1]

    # The mergers should have added their values together
    @test row.data[:merger] == Merger(110)

    # Normal collisions will have been overwritten.
    @test row.data[:meta] == 2
    @test row.data[:misc] == 0

    # Finally, test that we get an exception if we have multiple matches.
    S[(a = 1, b = 3)] = data3
    @test_throws ErrorException S[a = 1] = data3
end

