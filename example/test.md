<button type="button" onclick="toggleTheme()">Toggle Theme</button>
```js title="budi.md" caption="This is super random"
function Item({ name, isPacked }) {
  if (isPacked) {
    return null;
  }
  return <li className="item">{name}</li>;
}

export default function PackingList() {
  return (
    <section>
      <h1>Sally Ride's Packing List</h1>
      <ul>
        <Item
          isPacked={true}
          name="Space suit"
        />
        <Item
          isPacked={true}
          name="Helmet with a golden leaf"
        />
        <Item
          isPacked={false}
          name="Photo of Tam"
        />
      </ul>
    </section>
  );
}
```

```ocaml

utop # type person = { name: string ; age: int ; human : bool };;
type person = { name : string; age : int; human : bool; }

utop # let david = { name = "david" ; age = 32 ; human = false };;
val david : person = {name = "david"; age = 32; human = false}

utop # let mary = { david with name = "mary" };;
val mary : person = {name = "mary"; age = 32; human = false}

utop # let toggle_human (p : person) = { p with human = not p.human };;
val toggle_human : person -> person = <fun>

utop # let () =
  let david' = toggle_human david in
  if david'.human then
    print_endline "david is human"
  else
    print_endline "david is not human"
;;
david is human

(** From https://o1-labs.github.io/ocamlbyexample/basics-records.html *)
```