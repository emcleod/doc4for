import unittest
from typing import Dict, Any
from doc4for.f90.generate_type_tree import transform_inheritance_tree

class TestTransformInheritanceTree(unittest.TestCase):
    def test_empty_tree(self):
        inheritance_tree: Dict[str, Any] = {}
        expected_output = []
        self.assertEqual(transform_inheritance_tree(inheritance_tree), expected_output)

    def test_single_node_tree(self):
        inheritance_tree = {"A": {"children": [], "parent": None}}
        expected_output = [{"name": "A", "children": []}]
        self.assertEqual(transform_inheritance_tree(inheritance_tree), expected_output)

    def test_linear_tree(self):
        inheritance_tree = {
            "A": {"children": ["B"], "parent": None},
            "B": {"children": ["C"], "parent": "A"},
            "C": {"children": [], "parent": "B"},
        }
        expected_output = [
            {
                "name": "A",
                "children": [
                    {"name": "B", "children": [{"name": "C", "children": []}]}
                ],
            }
        ]
        self.assertEqual(transform_inheritance_tree(inheritance_tree), expected_output)

    def test_branching_tree(self):
        inheritance_tree = {
            "A": {"children": ["B", "C"], "parent": None},
            "B": {"children": ["D"], "parent": "A"},
            "C": {"children": ["E", "F"], "parent": "A"},
            "D": {"children": [], "parent": "B"},
            "E": {"children": [], "parent": "C"},
            "F": {"children": [], "parent": "C"},
        }
        expected_output = [
            {
                "name": "A",
                "children": [
                    {"name": "B", "children": [{"name": "D", "children": []}]},
                    {
                        "name": "C",
                        "children": [
                            {"name": "E", "children": []},
                            {"name": "F", "children": []},
                        ],
                    },
                ],
            }
        ]
        self.assertEqual(transform_inheritance_tree(inheritance_tree), expected_output)

    def test_multiple_root_nodes(self):
        inheritance_tree = {
            "A": {"children": ["B"], "parent": None},
            "B": {"children": [], "parent": "A"},
            "C": {"children": ["D"], "parent": None},
            "D": {"children": [], "parent": "C"},
        }
        expected_output = [
            {"name": "A", "children": [{"name": "B", "children": []}]},
            {"name": "C", "children": [{"name": "D", "children": []}]},
        ]
        self.assertEqual(transform_inheritance_tree(inheritance_tree), expected_output)

    def test_cyclic_dependency(self):
        inheritance_tree = {
            "A": {"children": ["B"], "parent": "C"},
            "B": {"children": ["C"], "parent": "A"},
            "C": {"children": ["A"], "parent": "B"},
        }
        expected_output = [
            {
                "name": "A",
                "children": [
                    {"name": "B", "children": [{"name": "C", "children": []}]}
                ],
            }
        ]
        self.assertEqual(transform_inheritance_tree(inheritance_tree), expected_output)


if __name__ == "__main__":
    unittest.main()
