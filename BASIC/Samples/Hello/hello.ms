function main()
{
  let name = readLine("Name:")
  print("Hello " + name + "!")
}

function readLine(prompt: string): string
{
  print(prompt)
  let name = input()
  return name
}
