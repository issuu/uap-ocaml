open Base
module Format = Caml.Format

module Parser = struct
  module Rule = struct
    type t = {
      regex: Re.re;
      replacements: (string * string) list;
      pattern: string;
    } [@@deriving show]

    let of_yaml: Yaml.value -> t = function
      | `O assoc -> 
        let pattern =
          List.Assoc.find_exn assoc ~equal:String.equal "regex"
          |> (function `String s -> s | _ -> failwith "invalid yaml")
        in
        let flags =
          List.Assoc.find assoc ~equal:String.equal "regex_flag"
          |> function Some `String "i" -> [`CASELESS] | _ -> []
        in
        let regex = Re.Pcre.regexp ~flags pattern in
        let replacements = List.fold_right assoc ~init:[] ~f:(fun o acc ->
          match o with
          | ("regex", _) -> acc 
          | ("regex_flag", _) -> acc 
          | (k, `String v) -> (k, v) :: acc
          | _ -> failwith "invalid yaml")
        in
        { regex; replacements; pattern }
      | _ -> failwith "invalid yaml"

    let apply t s =
      match Re.exec_opt t.regex s with
      | Some groups -> Some groups
      | None -> None
  end

  module Result = struct
    type groups = string list [@@deriving show, eq]
    type replacers = (string * string) list [@@deriving show, eq]
    type t = groups * replacers [@@deriving show, eq]

    let replace_regex = Re.Pcre.regexp "\\$\\d+"

    let replace s matches =
      Re.replace replace_regex s ~all:true ~f:(fun m ->
        Option.value ~default:"" @@ match (Re.Group.get m 0) with
        | "$1" -> List.nth matches 1
        | "$2" -> List.nth matches 2
        | "$3" -> List.nth matches 3
        | "$4" -> List.nth matches 4
        | "$5" -> List.nth matches 5
        | "$6" -> List.nth matches 6
        | "$7" -> List.nth matches 7
        | "$8" -> List.nth matches 8
        | "$9" -> List.nth matches 9
        | _ -> None)

    let get (groups, replacers) (s, idx) =
      let r = match List.Assoc.find replacers  ~equal:String.equal s with
        | Some rep -> Some (replace rep groups)
        | None -> List.nth groups idx
      in
      Option.map r ~f:String.strip |> function Some "" -> None | x -> x
  end

  type t = Rule.t list [@@deriving show]

  let of_yaml: Yaml.value -> t = function
    | `A seq -> List.map seq ~f:Rule.of_yaml
    | _ -> failwith "invalid yaml"
  
  let apply: t -> string -> Result.t = fun t s ->
    List.find_map t ~f:(fun rule ->
      match Rule.apply rule s with
      | Some groups -> Some (Re.Group.all groups |> Array.to_list, rule.replacements)
      | None -> None)
    |> Option.value ~default:([ "Other" ], [])
end


module UAParser = struct
  type t = Parser.t

  type result = {
    family: string;
    major: string option;
    minor: string option;
    patch: string option;
  } [@@deriving eq, show]

  let init () =
    match Yaml.of_string_exn [%blob "../uap-core/regexes.yaml"] with
    | `O assoc ->
        List.Assoc.find_exn assoc ~equal:String.equal "user_agent_parsers"
        |> Parser.of_yaml
    | _ -> failwith "invalid yaml"

  let parse t ua =
    let result = Parser.apply t ua in
    {
      family = Parser.Result.get result ("family_replacement", 1) |> Option.value ~default:"Other";
      major = Parser.Result.get result ("v1_replacement", 2);
      minor = Parser.Result.get result ("v2_replacement", 3);
      patch = Parser.Result.get result ("v3_replacement", 4);
    }
end

module OSParser = struct
  type t = Parser.t

  type result = {
    family: string;
    major: string option;
    minor: string option;
    patch: string option;
    patch_minor: string option;
  } [@@deriving eq, show]

  let init () =
    match Yaml.of_string_exn [%blob "../uap-core/regexes.yaml"] with
    | `O assoc ->
        List.Assoc.find_exn assoc ~equal:String.equal "os_parsers"
        |> Parser.of_yaml
    | _ -> failwith "invalid yaml"

  let parse t ua =
    let result = Parser.apply t ua in
    {
      family = Parser.Result.get result ("os_replacement", 1) |> Option.value ~default:"Other";
      major = Parser.Result.get result ("os_v1_replacement", 2);
      minor = Parser.Result.get result ("os_v2_replacement", 3);
      patch = Parser.Result.get result ("os_v3_replacement", 4);
      patch_minor = Parser.Result.get result ("os_v4_replacement", 5);
    }
end

module DeviceParser = struct
  type t = Parser.t

  type result = {
    family: string;
    brand: string option;
    model: string option;
  } [@@deriving eq, show]

  let init () =
    match Yaml.of_string_exn [%blob "../uap-core/regexes.yaml"] with
    | `O assoc ->
        List.Assoc.find_exn assoc ~equal:String.equal "device_parsers"
        |> Parser.of_yaml
    | _ -> failwith "invalid yaml"

  let parse t ua =
    let result = Parser.apply t ua in
    {
      family = Parser.Result.get result ("device_replacement", 1) |> Option.value ~default:"Other";
      brand = Parser.Result.get result ("brand_replacement", 2);
      (* Not a bug, model replacement is supposed to be 1, see JS ref implementation *)
      model = Parser.Result.get result ("model_replacement", 1);
    }
end
