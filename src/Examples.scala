object Examples {

  /*
    * . means any valid char can match
    * | manes any one of the options is valid
    * ? means valid with or without

    Ex1:
    pattern? ((h|j)ell. worl?d)|(42)
    string? hello world
    match
    string? jello word
    match
    string? jelly word
    match
    string? 42
    match
    string? 24
    no match
    string? hello world42
    no match

    Ex 2:
    pattern? I (like|love|hate) (cat|dog)? people
    string? I like cat people
    match
    string? I love dog people
    match
    string? I hate people
    match
    string? I likel people
    no match
    string? I people
    no match
   */
}
