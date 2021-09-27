# homelyapp

HomelyApp is Web applicatoin of household expenses.

(This project is my sandbox for Haskell with Bazel)

## Example

```
$ bazelisk run //:homelyapp -- $(pwd)/example/.homely.yaml
```

## Build

```
$ bazelisk run //:generateElm -- $(pwd)
$ bazelisk build //:homelyapp
```

## Development

Generate bazel config by [hazell](https://github.com/matsubara0507/hazell).
