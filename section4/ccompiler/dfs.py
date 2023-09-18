"""This module is for quick and dirty graph analysis using depth first search."""

from typing import Dict, Iterable, NewType
import pdb

Node = NewType("Node", str)
Route = NewType("Route", list[Node])

def depth_first_search(starting_node: Node, destination_node: Node, arcs: Dict[Node, list[Node]], visited_nodes_stack: list[Node]) -> Iterable[Route]:
    directly_reachable_unvisited_nodes: list[Node] = [ node for node in arcs[starting_node] if node not in visited_nodes_stack ]
    #print(f"directly_reachable_unvisited_nodes: {directly_reachable_unvisited_nodes}")
    pdb.set_trace()

    if destination_node in directly_reachable_unvisited_nodes:
        full_route = Route(visited_nodes_stack + [destination_node])
        yield full_route
        directly_reachable_unvisited_nodes.remove(destination_node)

    if len(directly_reachable_unvisited_nodes) > 0:
        for new_starting_node in directly_reachable_unvisited_nodes:
            new_visited_nodes_stack = visited_nodes_stack + [new_starting_node]
            ending_routes_from_node = depth_first_search(new_starting_node, destination_node, arcs, new_visited_nodes_stack)
            for route in ending_routes_from_node:
                yield route

def detect_cycles_in_graph_from_node_to_self(node: Node, arcs):

    directly_reachable_nodes = arcs[node]
    routes_list_unflattened: list[Iterable[Route]] = [ depth_first_search(directly_reachable_node, node, arcs, [directly_reachable_node]) for directly_reachable_node in directly_reachable_nodes ]
    routes_list: list[Route] = [ Route([node, ]) + route for iter_routes in routes_list_unflattened for route in iter_routes ]
    for route in routes_list:
        pretty_print_route(route)
    return routes_list
    

def pretty_print_route(route: Route) -> str:
    nodes_joined: str = " -> ".join(route)
    print(nodes_joined)
    return nodes_joined


nodes = [ Node(letter) for letter in ["a", "b", "c", "d", "e"] ]

arcs = {
    "a": ["b", "c"],
    "b": ["c", "d"],
    "c": ["a", "d"],
    "d": ["e"],
    "e": ["a"]
}

#ast_type_arcs = {
#    "function-definition": ["declaration-specifiers", "declarator", "declaration-list", "compound-statement"],
#    "declaration-list": ["declaration"],
#    "declarator": ["pointer", "direct-declarator"],
#    "direct-declarator": 


routes = detect_cycles_in_graph_from_node_to_self(Node("a"), arcs)

def main():
    routes = detect_cycles_in_graph_from_node_to_self(Node("a"), arcs)
    return routes
