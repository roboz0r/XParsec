const Colour = Object.freeze({ Red: 1, Green: 2 });
const onlyEnum = (x) => x;
const c = onlyEnum(Colour.Green);
(void (c));
