//----------------------------------------------------------------------------------------------------//
// MIT License
//
// Copyright (c) 2019 Alex Gamper
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//----------------------------------------------------------------------------------------------------//

using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdaGrammar
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine("Error: No arguments given");
                Console.WriteLine("Usage: AdaGrammar.exe <filename>");
                return;
            }

            StreamReader stream = null;

            try
            {
                stream = new StreamReader(args[0]);
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error: {0}", ex.Message);
                return;
            }

            Stopwatch stopwatch = Stopwatch.StartNew();

            using (stream)
            {
                try
                {
                    var input = new Antlr4.Runtime.AntlrInputStream(stream);
                    AdaLexer lexer = new AdaLexer(input);
                    CommonTokenStream tokens = new CommonTokenStream(lexer);
                    AdaParser parser = new AdaParser(tokens);

                    var result = parser.compilation_unit();

                }
                finally
                {
                    stream.Close();
                }
            }

            stopwatch.Stop();
            Console.WriteLine("Parse Elapsed time: {0}", stopwatch.Elapsed);
        }
    }
}
