function main()
{
  let x = string(true)
  let y = int("42")
  let z = any("text")
  let a = any(false)
}

function readLine(prompt: string): string
{
  print(prompt)
  let name = input()
  return name
}
