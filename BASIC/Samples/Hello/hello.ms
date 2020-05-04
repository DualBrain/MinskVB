function main()
{
  let x = 1 == 2
  let y = 1 != 2

  let a = "1" == "2"
  let b = "1" != "2"

  print(string(x))
  print(string(y))
  print(string(a))
  print(string(b))

}

function readLine(prompt: string): string
{
  print(prompt)
  let name = input()
  return name
}
