
import Nat "mo:base/Nat";
import HashMap "mo:base/HashMap";
import Hash "mo:base/Hash";
import Principal "mo:base/Principal";
import Random "mo:base/Random";
import Blob "mo:base/Blob";
import Array "mo:base/Array";
import Result "mo:base/Result";
import Iter "mo:base/Iter";
import List "mo:base/List";
import Nat32 "mo:base/Nat32";

actor {
  type Seller = {
    name : Text;
    service : Text;
    location : Text;
    rate : Nat.Nat;
  };

  type User = {
    name : Text;
    location : Text;
  };

  type Order = {
    user : User;
    seller : Seller;
    service : Text;
    rate : Nat.Nat;
  };

  type SellerRating = {
    user : User;
    seller : Seller;
    rating : Nat.Nat;
  };

  type UserRating = {
    user : User;
    seller : Seller;
    rating : Nat.Nat;
  };

  // ================Argument Types================
  type CreateSellerArgs = {
    name : Text;
    service : Text;
    location : Text;
    rate : Nat.Nat;
    principal : ?Principal;
  };

  type GetSellerArgs = {
    principal : Principal;
  };

  type GetSellerByNameArgs = {
    name : Text;
  };

  type GetSellerByLocationArgs = {
    location : Text;
  };

  type GetSellerByServiceArgs = {
    service : Text;
  };

  type GetSellerRatingByNameArgs = {
    name : Text;
  };

  type CreateUserArgs = {
    name : Text;
    location : Text;
  };

  type GetUserArgs = {
    principal : Principal;
  };

  type GetUserNameArgs = {
    name : Text;
  };

  type GetUserRatingByNameArgs = {
    name : Text;
  };

  type CreateOrderArgs = {
    userName : Text;
    sellerName : Text;
    service : Text;
    rate : Nat.Nat;
  };

  type CreateSellerRatingArgs = {
    userName : Text;
    sellerName : Text;
    rating : Nat.Nat;
  };

  type CreateUserRatingArgs = {
    userName : Text;
    sellerName : Text;
    rating : Nat.Nat;
  };

  // ================Hash Functions================
  private func customHash(n : Nat) : Hash.Hash {
    Nat32.fromNat(n);
  };

  // ================State================
  var sellers = HashMap.HashMap<Principal.Principal, Seller>(10, Principal.equal, Principal.hash);
  var users = HashMap.HashMap<Principal.Principal, User>(10, Principal.equal, Principal.hash);
  var orders = HashMap.HashMap<Nat, Order>(10, Nat.equal, customHash);
  var sellerRatings = HashMap.HashMap<Nat, SellerRating>(10, Nat.equal, customHash);
  var userRatings = HashMap.HashMap<Nat, UserRating>(10, Nat.equal, customHash);

  // ================Stable State================
  stable var sellersList = List.nil<(Principal.Principal, Seller)>();
  stable var usersList = List.nil<(Principal.Principal, User)>();
  stable var ordersList = List.nil<(Nat, Order)>();
  stable var sellerRatingsList = List.nil<(Nat, SellerRating)>();
  stable var userRatingsList = List.nil<(Nat, UserRating)>();

  // ================Functions================

  private func get_random_principal() : async Principal {
    let random_data = await Random.blob();
    return Principal.fromBlob(Blob.fromArray(Array.subArray(Blob.toArray(random_data), 0, 29)));
  };

  system func preupgrade() {
    sellersList := Iter.toList(sellers.entries());
    usersList := Iter.toList(users.entries());
    ordersList := Iter.toList(orders.entries());
    sellerRatingsList := Iter.toList(sellerRatings.entries());
    userRatingsList := Iter.toList(userRatings.entries());
  };

  system func postupgrade() {
    sellers := HashMap.fromIter<Principal.Principal, Seller>(List.toIter(sellersList), List.size(sellersList), Principal.equal, Principal.hash);
    users := HashMap.fromIter(List.toIter(usersList), List.size(usersList), Principal.equal, Principal.hash);
    orders := HashMap.fromIter(List.toIter(ordersList), List.size(ordersList), Nat.equal, customHash);
    sellerRatings := HashMap.fromIter(List.toIter(sellerRatingsList), List.size(sellerRatingsList), Nat.equal, customHash);
    userRatings := HashMap.fromIter(List.toIter(userRatingsList), List.size(userRatingsList), Nat.equal, customHash);
  };

  // ================Seller Functions================
  public func create_seller(args : CreateSellerArgs) : async Seller {
    let seller : Seller = {
      name = args.name;
      service = args.service;
      location = args.location;
      rate = args.rate;
    };
    switch (args.principal) {
      case (null) {
        sellers.put(await get_random_principal(), seller);
      };
      case (?principal) {
        sellers.put(principal, seller);
      };
    };
    seller;
  };

  public query func get_seller(args : GetSellerArgs) : async Result.Result<Seller, Text> {
    switch (sellers.get(args.principal)) {
      case (null) {
        #err("Seller not found");
      };
      case (?seller) {
        #ok(seller);
      };
    };
  };

  public query func get_all_sellers() : async [Seller] {
    Iter.toArray(sellers.vals());
  };

  public query func get_seller_by_name(args : GetSellerByNameArgs) : async Result.Result<Seller, Text> {
    for (seller in sellers.vals()) {
      if (seller.name == args.name) {
        return #ok(seller);
      };
    };
    #err("Seller not found");
  };

  public query func get_sellers_by_location(args : GetSellerByLocationArgs) : async [Seller] {
    Iter.toArray(
      Iter.filter<Seller>(
        sellers.vals(),
        func(seller : Seller) {
          seller.location == args.location;
        },
      )
    );
  };

  public query func get_seller_by_service(args : GetSellerByServiceArgs) : async [Seller] {
    Iter.toArray(
      Iter.filter<Seller>(
        sellers.vals(),
        func(seller : Seller) {
          seller.service == args.service;
        },
      )
    );
  };

  public query func get_sellers_by_rate(args : Nat.Nat) : async [Seller] {
    Iter.toArray(
      Iter.filter<Seller>(
        sellers.vals(),
        func(seller : Seller) {
          seller.rate >= args;
        },
      )
    );
  };

  // ================User Functions================
  public func create_user(name : Text, location : Text) : async User {
    let user : User = {
      name = name;
      location = location;
    };
    users.put(await get_random_principal(), user);
    user;
  };

  public query func get_user(args : GetUserArgs) : async Result.Result<User, Text> {
    switch (users.get(args.principal)) {
      case (null) {
        #err("User not found");
      };
      case (?user) {
        #ok(user);
      };
    };
  };

  public query func get_user_by_name(args : GetUserNameArgs) : async Result.Result<User, Text> {
    for (user in users.vals()) {
      if (user.name == args.name) {
        return #ok(user);
      };
    };
    #err("User not found");
  };

  public query func get_all_users() : async [User] {
    Iter.toArray(users.vals());
  };

  // ================Order Functions================

  public func create_order(args : CreateOrderArgs) : async Result.Result<Order, Text> {
    let user = await get_user_by_name({ name = args.userName });
    let seller = await get_seller_by_name({ name = args.sellerName });
    switch ((user, seller)) {
      case ((#ok(user), #ok(seller))) {
        let order : Order = {
          user = user;
          seller = seller;
          service = args.service;
          rate = args.rate;
        };
        orders.put(orders.size() + 1, order);
        #ok(order);

      };
      case ((_, _)) {
        #err("User or Seller not found");
      };
    };
  };

  public query func get_all_orders() : async [Order] {
    Iter.toArray(orders.vals());
  };

  // ================Rating Functions================

  public func create_seller_rating(args : CreateSellerRatingArgs) : async Result.Result<SellerRating, Text> {
    let user = await get_user_by_name({ name = args.userName });
    let seller = await get_seller_by_name({ name = args.sellerName });
    switch ((user, seller)) {
      case ((#ok(user), #ok(seller))) {
        let sellerRating : SellerRating = {
          user = user;
          seller = seller;
          rating = args.rating;
        };
        sellerRatings.put(sellerRatings.size() + 1, sellerRating);
        #ok(sellerRating);

      };
      case ((_, _)) {
        #err("User or Seller not found");
      };
    };
  };

  public query func get_seller_rating_by_name(args : GetSellerRatingByNameArgs) : async [SellerRating] {
    Iter.toArray(
      Iter.filter<SellerRating>(
        sellerRatings.vals(),
        func(rating : SellerRating) {
          rating.seller.name == args.name;
        },
      )
    );
  };

  public query func get_all_seller_ratings() : async [SellerRating] {
    Iter.toArray(sellerRatings.vals());
  };

  public func create_user_rating(args : CreateUserRatingArgs) : async Result.Result<UserRating, Text> {
    let user = await get_user_by_name({ name = args.userName });
    let seller = await get_seller_by_name({ name = args.sellerName });
    switch ((user, seller)) {
      case ((#ok(user), #ok(seller))) {
        let userRating : UserRating = {
          user = user;
          seller = seller;
          rating = args.rating;
        };
        userRatings.put(userRatings.size() + 1, userRating);
        #ok(userRating);
      };
      case ((_, _)) {
        #err("User or Seller not found");
      };
    };
  };

  public query func get_user_rating_by_name(args : GetUserRatingByNameArgs) : async [UserRating] {
    Iter.toArray(
      Iter.filter<UserRating>(
        userRatings.vals(),
        func(rating : UserRating) {
          rating.user.name == args.name;
        },
      )
    );
  };

  public query func get_all_user_ratings() : async [UserRating] {
    Iter.toArray(userRatings.vals());
  };

  public query func get_all_ratings() : async ([SellerRating], [UserRating]) {
    (Iter.toArray(sellerRatings.vals()), Iter.toArray(userRatings.vals()));
  };

  public func get_all_ratings_by_name(args : GetSellerRatingByNameArgs) : async ([SellerRating], [UserRating]) {
    let seller = await get_seller_by_name({ name = args.name });
    switch (seller) {
      case (#ok(seller)) {

        let sellerR = Iter.toArray<SellerRating>(
          Iter.filter<SellerRating>(
            sellerRatings.vals(),
            func(rating : SellerRating) {
              rating.seller.name == args.name;
            },
          )
        );
        let userR = Iter.toArray(
          Iter.filter<UserRating>(
            userRatings.vals(),
            func(rating : UserRating) {
              rating.seller.name == args.name;
            },
          )
        );
        (sellerR, userR);
      };
      case (_) {
        return ([], []);
      };
    };
  };
};
